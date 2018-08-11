package scalene.routing

import scala.language.higherKinds

import scala.annotation.implicitNotFound
import scalene.Deferred

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._

class Cell[T](val index: Int) {
  def set(t: T, arr: VSet) {
    arr.set(index, t)
  }
  def get(arr: VSet): T = {
    arr[T](index)
  }
}

trait WrappedParser[I] {
  def parse(input: I, values: VSet): Result[Unit]

}

class WrappedParserImpl[I,O](parser: Parser[I,O], cell: Cell[O]) extends WrappedParser[I] {
  val ok: Result[Unit] = Right(())
  def parse(input: I, values: VSet): Result[Unit] = parser.parse(input) match {
    case Left(err) => Left(err)
    case Right(value) => {
      cell.set(value, values)
      ok
    }
  }
}

trait WrappedFilter[I] {
  def parse(input: I, values: VSet): Deferred[Unit]
}

class WrappedFilterImpl[I,O](filter: Filter[I,O], cell: Cell[O]) extends WrappedFilter[I] {
  val ok: Result[Unit] = Right(())
  def parse(input: I, values: VSet): Deferred[Unit] = filter(input) map {value =>
    cell.set(value, values)
    ()
  }
}

sealed trait CellComponent[I, O]
case class CellParser[I, O](parser: Parser[I, O]) extends CellComponent[I,O] {
  def wrapped(index: Int) : (WrappedParser[I], Cell[O]) = {
    val cell = new Cell[O](index)
    (new WrappedParserImpl(parser, cell), cell)
  }
}


case class CellFilter[I,O](filter: Filter[I,O]) extends CellComponent[I,O] {
  def wrapped(index: Int) : (WrappedFilter[I], Cell[O]) = {
    val cell = new Cell[O](index) 
    (new WrappedFilterImpl(filter, cell), cell)
  }
}

trait AsCellComponent[P[_,_]] {
  def apply[I,O](p: P[I,O]): CellComponent[I,O]
}

object AsCellComponent {

  implicit def liftParser = new AsCellComponent[Parser] {
    def apply[I,O](parser: Parser[I,O]): CellComponent[I,O] = CellParser(parser)
  }

  implicit def liftFilter = new AsCellComponent[Filter] {
    def apply[I,O](filter: Filter[I,O]) = CellFilter(filter)
  }
}

/**
 * This typeclass is used with AsCellComponent to turn a parser or a filter into a RouteBuilder 
 */
trait AsRouteBuilder[I <: Clonable[I], O, P[_,_]] {
  
  //needed because O is probably not a HList (maybe fix that?)
  type FixedOut <: HList 

  def apply(p: P[I,O]): RouteBuilder[I,FixedOut]
}

object AsRouteBuilder {

  implicit def liftAsCellComponent[I <: Clonable[I], O, P[_,_]](implicit as: AsCellComponent[P], fuse: Fuse[HNil, O]) = new AsRouteBuilder[I,O,P] {
    type FixedOut = fuse.Out
    def apply(p: P[I,O]): RouteBuilder[I,FixedOut] = RouteBuilder.cons(RouteBuilder.nil[I], as(p))
  }

  type Aux[I <: Clonable[I], O, P[_,_], L <: HList] = AsRouteBuilder[I,O,P]{ type FixedOut = L }

}

//used when making phantom clones of QLists for dealing with subroute trees
case class CellPhantom[I,O](cell: Cell[O]) extends CellComponent[I,O]

/**
 * A RouteBuilder has three responsibilities.  First, it stores all the type
 * information of a route, aka the HList of extracted values.  Second, it
 * builds the RouteExecutor which handles the actual execution of the parsers/filters.
 * Lastly, it takes the VSet of extracted values produced by its RouteExecutor and
 * constructs the output HList.
 */
trait RouteBuilder[I <: Clonable[I], L <: HList] {
  def buildRouteExecutor : RouteExecutor[I]
  //def buildResolver: VSet => L
  def build(values: VSet): L

  def size: Int

  def shallowClone: RouteBuilder[I,L]
}
object RouteBuilder {

  def nil[I <: Clonable[I]] = new RouteBuilder[I, HNil] {
    def buildRouteExecutor = RouteExecutor[I](Nil, Nil)
    def build(values: VSet) = HNil

    val size = 0
    def shallowClone: RouteBuilder[I,HNil] = this
  }

  def cons[I <: Clonable[I],O, L <: HList](prev : RouteBuilder[I, L], next: CellComponent[I,O])(implicit fuse: Fuse[L, O]): RouteBuilder[I, fuse.Out] = new RouteBuilder[I, fuse.Out] {

    val size = prev.size + 1
    val prevList = prev.buildRouteExecutor

    val (slist, cell) = next match {
      case p @ CellParser(_) => {
        val (wrapped, cell) = p.wrapped(size - 1)
        (RouteExecutor(prevList.parsers :+ wrapped, prevList.filters), cell)
      }
      case f @ CellFilter(_) => {
        val (wrapped, cell) = f.wrapped(size - 1)
        (RouteExecutor(prevList.parsers, prevList.filters :+ wrapped), cell)
      }
      case CellPhantom(cell) => {
        (RouteExecutor(prevList.parsers, prevList.filters), cell)
      }
    }
    val buildRouteExecutor = slist
    def build(values: VSet): fuse.Out = fuse.fuse(prev.build(values), cell.get(values))

    def shallowClone = cons(prev.shallowClone, CellPhantom(cell))

  }

  def mapped[I <: Clonable[I], L <: HList, O](nested: RouteBuilder[I, L], map: L => O)(implicit f: Fuse[HNil, O]): RouteBuilder[I, f.Out] = new RouteBuilder[I,f.Out] {

    val size = nested.size 
    def buildRouteExecutor = nested.buildRouteExecutor

    def build(values: VSet): f.Out = f.fuse(HNil, map(nested.build(values)))

    def shallowClone = mapped(nested.shallowClone, map) //is this right?!?!?!
  }
}
