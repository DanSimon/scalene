package scalene.routing

import scala.language.higherKinds

import scala.annotation.implicitNotFound
import scalene._

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

//used when making phantom clones of QLists for dealing with subroute trees
case class CellPhantom[I,O](cell: Cell[O]) extends CellComponent[I,O]

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

//putting all this in a trait makes all the definitions simpler and also allows
//type inference to work properly on the `to` method.  It does mean you can't
//mix routes with different I/O, but that doesn't seem like a real use case.
//After all, not even any real reason to keep the I/O parameterized right now.
//
//NOTE - trying to make the `subroutes` and `to` operations into type classes
//doesn't work without messing up type inference if we want to have versions
//that accept both functions and constants
//
//The alternative is have two different versions of `to` such that the function
//version only works on non-HNil builders, but even that might not work unless
//we have two versions of RouteBuilder

trait RouteBuilding[I <: Clonable[I], FinalOut] {

  /*
   * A RouteExecutor represents a fully constructed route parser.  It handles the actual
   * execution of the routes parsers and filters, accumulating the extracted
   * values into a VSet.  A corrosponding RouteBuilder handles properly typing the
   * extracted values into a result HList
   */
  case class RouteExecutor(parsers: List[WrappedParser[I]], filters: List[WrappedFilter[I]]) {
    def size = parsers.size + filters.size

    private val unitRight = Right(())
    private val arr = parsers.toArray

    def executeParsers(input: I, values: VSet): Result[Unit] = {
      var res: Result[Unit] = unitRight
      var i = 0
      while (i < arr.length && res.isRight) {
        res = arr(i).parse(input, values)
        i += 1
      }
      res
    }


    def executeFilters(input: I, prevFilters: List[WrappedFilter[I]], values: VSet): Deferred[Unit] = {
      (prevFilters ++ filters).foldLeft[Deferred[Unit]](defer{ _ => Async.successful(())}){case (build, next) =>
        build.flatMap {_ =>
          next.parse(input, values)
        }
      }
    }
  }

  /**
   * This typeclass is used with AsCellComponent to turn a parser or a filter into a RouteBuilder 
   */
  trait AsRouteBuilder[O, P[_,_]] {
    
    //needed because O is probably not a HList (maybe fix that?)
    type FixedOut <: HList 

    def apply(p: P[I,O]): RouteBuilder[FixedOut]
  }

  object AsRouteBuilder {

    implicit def liftAsCellComponent[O, P[_,_], Out <: HList](implicit 
      as: AsCellComponent[P],
      fuse: Fuse.Aux[HNil, O, Out],
      tupler: Tupler[Out]
    ) = new AsRouteBuilder[O, P] {
      type FixedOut = Out
      def apply(p: P[I,O]): RouteBuilder[FixedOut] = RouteBuilder.cons(RouteBuilder.RNil, as(p))
    }

    type Aux[O, P[_,_], L <: HList] = AsRouteBuilder[O, P]{ type FixedOut = L }

  }


  /**
   * A RouteBuilder has three responsibilities.  First, it stores all the type
   * information of a route, aka the HList of extracted values.  Second, it
   * builds the RouteExecutor which handles the actual execution of the parsers/filters.
   * Lastly, it takes the VSet of extracted values produced by its RouteExecutor and
   * constructs the output HList.
   */
  trait RouteBuilder[L <: HList] {

    def tupler: Tupler[L]
    type Tupled = tupler.Out

    def buildRouteExecutor : RouteExecutor
    //def buildResolver: VSet => L
    def build(values: VSet): L

    def size: Int

    def shallowClone: RouteBuilder[L]

    def subroutes(subs: (RouteBuilder[L] => Route[I,FinalOut])*): Route[I,FinalOut] = {
      val slist = buildRouteExecutor
      val subroutes: Array[Route[I,FinalOut]] = subs.map{sub => sub(shallowClone)}.toArray
      val notFoundError: RouteResult[FinalOut] = Left(ParseError.notFound("no route"))
      new Route[I,FinalOut] {
        val vsetSize = subroutes.map{_.vsetSize}.max + size

        def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[FinalOut] = slist.executeParsers(input, values) flatMap {unit =>
          //at this point we know parsing is successful up to the subroute branching, now find the correct subroute(if any)
          val nextFilters = collectedFilters ++ slist.filters
          subroutes.foldLeft[RouteResult[FinalOut]](notFoundError){
            case (success @ Right(res), next) => success
            case (Left(err), next)  => err.reason match {
              case ErrorReason.NotFound   => next.execute(input.cclone, nextFilters, values)
              case ErrorReason.BadRequest => Left(err)
            }
          }
        }
      }

    }

    def to(completion: Tupled => Deferred[FinalOut]): Route[I,FinalOut] = {
      val slist = buildRouteExecutor
      new Route[I,FinalOut] {
        final val vsetSize = slist.size

        final def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[FinalOut] = slist.executeParsers(input, values) match {
          case Right(_) => Right (
            if (collectedFilters.isEmpty && slist.filters.isEmpty) {
              completion(tupler(build(values)))
            } else {
              slist.executeFilters(input, collectedFilters, values) flatMap { _ => 
                completion(tupler(build(values)))
              }
            }
          )
          case Left(err) => Left(err)
        }
      }
    }
    /*
    def to[N <: Nat](const: FinalOut)(implicit len: Length.Aux[L, N], nat: ToInt[N]): Route[I,FinalOut] = {
      to(_ => const)
    }

    def to[N <: Nat](const: Deferred[FinalOut])(implicit len: Length.Aux[L, N], nat: ToInt[N]): Route[I,FinalOut] = {
      to(_ => const)
    }
    */
  }
  object RouteBuilder {

    object RNil extends RouteBuilder[HNil] {
      
      val tupler = implicitly[Tupler[HNil]]

      def buildRouteExecutor = RouteExecutor(Nil, Nil)
      def build(values: VSet) = HNil

      val size = 0
      def shallowClone: RouteBuilder[HNil] = this
    }

    def cons[O, L <: HList, Out <: HList](prev : RouteBuilder[L], next: CellComponent[I,O])(implicit fuse: Fuse.Aux[L, O, Out], _tupler: Tupler[Out]): RouteBuilder[Out] = new RouteBuilder[Out] {

      val tupler = _tupler

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

    /*
    def mapped[O, L <: HList](nested: RouteBuilder[L], map: L => O)(implicit f: Fuse[HNil, O]): RouteBuilder[f.Out] = new RouteBuilder[f.Out] {

      val size = nested.size 
      def buildRouteExecutor = nested.buildRouteExecutor

      def build(values: VSet): f.Out = f.fuse(HNil, map(nested.build(values)))

      def shallowClone = mapped(nested.shallowClone, map) //is this right?!?!?!
    }
    */
  }
}
