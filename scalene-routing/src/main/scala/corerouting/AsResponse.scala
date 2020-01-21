package scalene.corerouting

import scala.annotation.implicitNotFound
import scalene.Deferred

@implicitNotFound("Result must be a ${Out}, Deferred[${Out}], or have an implicit AsResponse[${T}, ${Out}] in scope")
trait AsResponse[T, Out] {
  def apply(t: T): Deferred[Out]
}

trait LPAsResponse {

  implicit def liftIdentity[Out, In <: Out] = new AsResponse[In,Out] {

    @inline
    def apply(t: In): Deferred[Out] = Deferred.successful(t)
  }

  implicit def liftDeferredIdentity[Out, In <: Out] = new AsResponse[Deferred[In], Out] {
    @inline
    def apply(t: Deferred[In]): Deferred[Out] = t.map{x => x}
  }

}

object AsResponse extends LPAsResponse{


  implicit def identity[T] = new AsResponse[Deferred[T], T] {

    @inline
    def apply(d: Deferred[T]) = d
  }
}
