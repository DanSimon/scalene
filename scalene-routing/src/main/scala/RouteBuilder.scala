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

object Routes {

  /*
  def apply(routes: HttpRoute*): HttpRoute = {
    ???
    //RouteBuilder.nil[RequestContext].subroutes(routes.map{route => (n: RouteBuilder[RequestContext, HNil]) => route}: _*)
  }
  */

}






