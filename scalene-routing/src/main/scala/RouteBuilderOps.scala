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

//mixed in to routing package
trait RouteBuilderOps[I <: Clonable[I], FinalOut] { self: RouteBuilding[I,FinalOut] => 

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
    implicit def comCom[A, B , CA[_,_] , CB[_,_], O, FOut](
      implicit 
      nilFuse: Fuse.Aux[Unit, A, O],
      fuse: Fuse.Aux[O,B, FOut],
      asA: AsCellComponent[CA],
      asB: AsCellComponent[CB]
    ) = new RouteBuilderCombiner[CA[I,A], CB[I,B]] {
      type Out = RouteBuilder[FOut]
      def apply(a: CA[I,A], b: CB[I,B]): RouteBuilder[FOut] = {
        val f: RouteBuilder[O] = RouteBuilder.cons(RouteBuilder.RNil, asA(a))
        RouteBuilder.cons(f, asB(b))
      }
    }

    /**
     * Combine a buider with a thing that can become a cell component
     */
    implicit def builderCom[A , B , C[_,_], FOut](
      implicit fuse: Fuse.Aux[A,B, FOut],
      as: AsCellComponent[C]
    ) = new RouteBuilderCombiner[RouteBuilder[A], C[I,B]] {
      type Out = RouteBuilder[FOut]
      def apply(a: RouteBuilder[A], b: C[I,B]): RouteBuilder[FOut] = RouteBuilder.cons(a, as(b))
    }

  }

  object RouteBuilderCombiner extends LowPriorityRouteBuilderCombiners {

  }

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


  implicit class CombineTo[A](val a: A) {
    def +[B](b: B)(implicit com: RouteBuilderCombiner[A,B]): com.Out = com(a,b)
  }

}

object Tests {
  import httprouting._

  //val a = Parameter("a", ![Int]) + Parameter("b", ![Int]) to { case (a,b) => Body.plain((a * b).toString).ok }
  //

  trait Z[B] {

    case class Foo[A](a: A) {

      def to(f: A => B): B = f(a)

      //def to(b: B): B = b

    }


  }
  object ZS extends Z[String]
  import ZS._
  //Foo((3, 4)) to {case (a,b) => (a + b).toString}
  //Foo((4, "est")) to "hello"

  case class Bar[A, B](a: A) {

    def to(f: A => B): B = f(a)

    def to(b: B): B = b

  }
  //Bar[(Int, Int), String]((3, 4)) to {case Tuple2(a,b) => (a + b).toString}

  //Bar[Int :|: Int :|: HNil, String](3 :: 4 :: HNil) to {case a :|: b :|: HNil => (a + b).toString}

  case class Concrete(i: (Int, Int)) {

    def to(f: ((Int, Int)) => String): String = f(i)

    def to(s: String): String = s

  }

  //Concrete((3, 4)) to {case (a,b) => (a + b).toString}


}

