package scalene.routing

import shapeless.ops.tuple._

/**
 * Typeclass used to fuse together two things which may be HLists.  
 */
trait Fuse[A,B] {
  type Out
  def fuse(a: A, b: B): Out
}

trait LowestPriorityFuse {
  implicit def vFusev[A, B] = new Fuse[A,B] { 
    type Out = (A,B) 
    def fuse(a : A, b : B): Out = (a,b)
  }

}

trait LowPriorityFuse extends LowestPriorityFuse {
  implicit def vFuseP[A, B](implicit p: Prepend[Tuple1[A], B]) = new Fuse[A,B] { 
    type Out = p.Out
    def fuse(a : A, b : B): Out = p(Tuple1(a), b)
  }
  implicit def hFusev[A, B](implicit p: Prepend[A, Tuple1[B]]) = new Fuse[A,B] { 
    type Out = p.Out
    def fuse(a : A, b : B): Out = p(a, Tuple1(b))
  }

}

object Fuse extends LowPriorityFuse {
  
  type Aux[A,B, O] = Fuse[A,B] { type Out = O }

  implicit def hFuseh[A , B](implicit p: Prepend[A, B]) = new Fuse[A,B] { 
    type Out = p.Out
    def fuse(a : A, b : B): Out = p(a,b)
  }

}

