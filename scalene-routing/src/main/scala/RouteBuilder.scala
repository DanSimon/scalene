package router

import scala.annotation.implicitNotFound
import scala.language.higherKinds

import scalene._
import scalene.http.{Method => HttpMethod, _}
import shapeless.{:: => :|:, _}
import ops.hlist._
import ops.nat.ToInt
import syntax.std.function._
import ops.function._


class VSet(size: Int) {
  private val map = new Array[Any](math.max(size, 1))
  def set(index: Int, value: Any) {
    map(index) = value
  }
  def apply[T](index: Int): T = {
    map(index).asInstanceOf[T]
  }
}
object VSet {
  def apply(size: Int): VSet = new VSet(size)

  val empty = VSet(0)
}

trait Route[I,O] {
  
  def vsetSize: Int

  def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[O]

  final def apply(input: I): RouteResult[O] = execute(input, Nil, if (vsetSize == 0) VSet.empty else VSet(vsetSize))

  final def toRoute: I => Result[Deferred[O]] = input => apply(input)

  final def toCompleteRoute: I => Deferred[O] = input => apply(input) match {
    case Left(err) => defer{ _ => Async.failure(new Exception(s"Error: $err")) }
    case Right(f)  => f
  }

  def toFilter: Filter[I,O] = Filter(toCompleteRoute)
}
object Route {

  type Completion[I <: Clonable[I], F <: HList, O] = CellList[I, F] => Route[I, O]

  def apply[L <: HList](builder: Completion[RequestContext, L, HttpResponse]) = builder

}


/*
 * An SList represents a fully constructed route parser.  It handles the actual
 * execution of the routes parsers and filters, accumulating the extracted
 * values into a VSet.  A corrosponding CellList handles properly typing the
 * extracted values into a result HList
 */
case class SList[I <: Clonable[I]](parsers: List[WrappedParser[I]], filters: List[WrappedFilter[I]]) {
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
 * Typeclass to combine things into RouteBuilders.  Those things can either be
 * pairs of parser/filters or an existing routebuilder with another
 * parser/filter
 */
trait RouteBuilderCombiner[A,B] {
  type Out
  def apply(a: A, b: B): Out
}

trait LowPriorityRouteBuilderCombiners {

  /**
   * Combine two things that can become cell components, which would be either
   * a parser or a filter
   */
  implicit def comCom[I <: Clonable[I], A, B , CA[_,_] , CB[_,_], O <: HList](
    implicit 
    nilFuse: Fuse.Aux[HNil, A, O],
    fuse: Fuse[O,B],
    asA: AsCellComponent[CA],
    asB: AsCellComponent[CB]
  ) = new RouteBuilderCombiner[CA[I,A], CB[I,B]] {
    type Out = CellList[I, fuse.Out]
    def apply(a: CA[I,A], b: CB[I,B]): CellList[I,fuse.Out] = {
      val f: CellList[I,O] = CellList.cons(CellList.nil[I], asA(a))
      CellList.cons(f, asB(b))
    }
  }

  /**
   * Combine a buider with a thing that can become a cell component
   */
  implicit def builderCom[I <: Clonable[I], A <: HList, B , C[_,_]](
    implicit fuse: Fuse[A,B],
    as: AsCellComponent[C]
  ) = new RouteBuilderCombiner[CellList[I,A], C[I,B]] {
    type Out = CellList[I, fuse.Out]
    def apply(a: CellList[I,A], b: C[I,B]): CellList[I,fuse.Out] = CellList.cons(a, as(b))
  }



}

object RouteBuilderCombiner extends LowPriorityRouteBuilderCombiners {

  implicit def optimizedMethodUrlCombiner = new RouteBuilderCombiner[Method, Url] {
    type Out = Parser[RequestContext, HNil] //ExactMatchPath
    def apply(a: Method, b: Url) = new ExactMatchPath(a.method,b.url)
  }


}

object R {

  implicit class CombineTo[A](val a: A) {
    def +[B](b: B)(implicit com: RouteBuilderCombiner[A,B]): com.Out = com(a,b)
  }

  implicit class CellComponentOps[I <: Clonable[I], O, P[_,_], L <: HList](p: P[I,O])(implicit as: AsCellList.Aux[I,O, P, L]) extends CellListOps[I, L](as(p))

  implicit class CellListOps[I <: Clonable[I], L <: HList](val builder: CellList[I,L]) {
    def map[U](f: L => U)(implicit fuse: Fuse[HNil, U]): CellList[I,fuse.Out] = {
      CellList.mapped(builder, f)
    }

    private lazy val shallowClone = builder.shallowClone

    def subroutes[O](subs: (CellList[I, L] => Route[I,O])*): Route[I,O] = {
      val slist = builder.buildSList
      val subroutes = subs.map{sub => sub(shallowClone)}
      val notFoundError: RouteResult[O] = Left(ParseError.notFound("no route"))
      new Route[I,O] {
        val vsetSize = subroutes.map{_.vsetSize}.max + builder.size

        def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[O] = slist.executeParsers(input, values) flatMap {unit =>
          //at this point we know parsing is successful up to the subroute branching, now find the correct subroute(if any)
          val nextFilters = collectedFilters ++ slist.filters
          subroutes.foldLeft[RouteResult[O]](notFoundError){
            case (Right(res), next) => Right(res)
            case (Left(err), next)  => err.reason match {
              case ErrorReason.NotFound   => next.execute(input.cclone, nextFilters, values)
              case ErrorReason.BadRequest => Left(err)
            }
          }
        }
      }

    }

    def to[O, N <: Nat](completion: L => Deferred[O])(implicit len: Length.Aux[L, N], nat: ToInt[N]): Route[I,O] = {
      val slist = builder.buildSList
      new Route[I,O] {
        final val vsetSize = nat()

        final def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[O] = slist.executeParsers(input, values) match {
          case Right(_) => Right (
            if (collectedFilters.isEmpty && slist.filters.isEmpty) {
              completion(builder.build(values))
            } else {
              slist.executeFilters(input, collectedFilters, values) flatMap { _ => 
                completion(builder.build(values))
              }
            }
          )
          case Left(err) => Left(err)
        }
      }
    }

  }
}


