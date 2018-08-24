package scalene.corerouting

import scala.annotation.implicitNotFound
import scalene.Deferred

@implicitNotFound("Result must be a ${Out}, Deferred[${Out}], or have an implicit AsResponse[${T}, ${Out}] in scope")
trait AsResponse[T, Out] {
  def apply(t: T): Deferred[Out]
}

trait LPAsResponse {

  implicit def liftIdentity[T] = new AsResponse[T,T] {

    @inline
    def apply(t: T) = Deferred.successful(t)
  }

}

object AsResponse extends LPAsResponse{


  implicit def identity[T] = new AsResponse[Deferred[T], T] {

    @inline
    def apply(d: Deferred[T]) = d
  }
}
