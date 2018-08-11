package scalene.routing

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

  type Completion[I <: Clonable[I], F <: HList, O] = RouteBuilder[I, F] => Route[I, O]

  def apply[L <: HList](builder: Completion[RequestContext, L, HttpResponse]) = builder

}

object Routes {

  def apply(routes: HttpRoute*): HttpRoute = {
    RouteBuilder.nil[RequestContext].subroutes(routes.map{route => (n: RouteBuilder[RequestContext, HNil]) => route}: _*)
  }

}



/*
 * A RouteExecutor represents a fully constructed route parser.  It handles the actual
 * execution of the routes parsers and filters, accumulating the extracted
 * values into a VSet.  A corrosponding RouteBuilder handles properly typing the
 * extracted values into a result HList
 */
case class RouteExecutor[I <: Clonable[I]](parsers: List[WrappedParser[I]], filters: List[WrappedFilter[I]]) {
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



