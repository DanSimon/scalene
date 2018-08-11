package scalene.routing

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._

/**
 * Typeclass used to fuse together two things which may be HLists.  
 */
trait Fuse[A,B] {
  type Out <: HList
  def fuse(a: A, b: B): Out
}

trait LowestPriorityFuse {
  implicit def vFusev[A, B] = new Fuse[A,B] { 
    type Out = A :|: B :|: HNil 
    def fuse(a : A, b : B): Out = a :: b :: HNil
  }

}

trait LowPriorityFuse extends LowestPriorityFuse {
  implicit def vFuseH[A, B <: HList] = new Fuse[A,B] { 
    type Out = A :|: B 
    def fuse(a : A, b : B): Out = a :: b
  }
  implicit def hFusev[A <: HList, B](implicit p: Prepend[A, B :|: HNil]) = new Fuse[A,B] { 
    type Out = p.Out
    def fuse(a : A, b : B): Out = a :+ b
  }

}

object Fuse extends LowPriorityFuse {
  
  type Aux[A,B, O <: HList] = Fuse[A,B] { type Out = O }

  implicit def hFuseh[A <: HList, B <: HList](implicit p: Prepend[A, B]) = new Fuse[A,B] { 
    type Out = p.Out
    def fuse(a : A, b : B): Out = a ++ b
  }

}

