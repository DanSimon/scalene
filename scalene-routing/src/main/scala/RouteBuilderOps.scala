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
    type Out = RouteBuilder[I, fuse.Out]
    def apply(a: CA[I,A], b: CB[I,B]): RouteBuilder[I,fuse.Out] = {
      val f: RouteBuilder[I,O] = RouteBuilder.cons(RouteBuilder.nil[I], asA(a))
      RouteBuilder.cons(f, asB(b))
    }
  }

  /**
   * Combine a buider with a thing that can become a cell component
   */
  implicit def builderCom[I <: Clonable[I], A <: HList, B , C[_,_]](
    implicit fuse: Fuse[A,B],
    as: AsCellComponent[C]
  ) = new RouteBuilderCombiner[RouteBuilder[I,A], C[I,B]] {
    type Out = RouteBuilder[I, fuse.Out]
    def apply(a: RouteBuilder[I,A], b: C[I,B]): RouteBuilder[I,fuse.Out] = RouteBuilder.cons(a, as(b))
  }

}

object RouteBuilderCombiner extends LowPriorityRouteBuilderCombiners {
  implicit def optimizedMethodUrlCombiner = new RouteBuilderCombiner[Method, Url] {
    type Out = Parser[RequestContext, HNil] //ExactMatchPath
    def apply(a: Method, b: Url) = new ExactMatchPath(a.method,b.url)
  }
}

@implicitNotFound("Result must be a ${Out}, Deferred[${Out}], or have an implicit AsResponse[${T}, ${Out}] in scope")
trait AsResponse[T, Out] {
  def apply(t: T): Deferred[Out]
}

object AsResponse {

  implicit def liftIdentity[T] = new AsResponse[T,T] {

    @inline
    def apply(t: T) = Deferred.successful(t)
  }

  implicit def identity[T] = new AsResponse[Deferred[T], T] {

    @inline
    def apply(d: Deferred[T]) = d
  }
}


//mixed in to routing package
trait RouteBuilderOps[FinalOut] {

  implicit class CombineTo[A](val a: A) {
    def +[B](b: B)(implicit com: RouteBuilderCombiner[A,B]): com.Out = com(a,b)
  }

  implicit class CellComponentOps[I <: Clonable[I], O, P[_,_], L <: HList]
    (p: P[I,O])
    (implicit as: AsRouteBuilder.Aux[I,O, P, L])
    extends RouteBuilderOps[I, L](as(p))

  implicit class RouteBuilderOps[I <: Clonable[I], L <: HList](val builder: RouteBuilder[I,L]) {
    def map[U](f: L => U)(implicit fuse: Fuse[HNil, U]): RouteBuilder[I,fuse.Out] = {
      RouteBuilder.mapped(builder, f)
    }

    private lazy val shallowClone = builder.shallowClone

    def subroutes[O](subs: (RouteBuilder[I, L] => Route[I,O])*): Route[I,O] = {
      val slist = builder.buildRouteExecutor
      val subroutes: Array[Route[I,O]] = subs.map{sub => sub(shallowClone)}.toArray
      val notFoundError: RouteResult[O] = Left(ParseError.notFound("no route"))
      new Route[I,O] {
        val vsetSize = subroutes.map{_.vsetSize}.max + builder.size

        def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[O] = slist.executeParsers(input, values) flatMap {unit =>
          //at this point we know parsing is successful up to the subroute branching, now find the correct subroute(if any)
          val nextFilters = collectedFilters ++ slist.filters
          var res: RouteResult[O] = notFoundError
          var nextInput = input
          var i = 0
          while (i < subroutes.length) {
            subroutes(i).execute(nextInput, nextFilters, values) match {
              case success @ Right(_) => {
                res = success
                i = subroutes.length + 1
              }
              case Left(err) => err.reason match {
                case ErrorReason.NotFound   => {
                  i += 1
                  nextInput = input.cclone
                }
                case ErrorReason.BadRequest => {
                  res = Left(err)
                  i = subroutes.length + 1
                }
              }
            }
          }
          res
          /*
          subroutes.foldLeft[RouteResult[O]](notFoundError){
            case (success @ Right(res), next) => success
            case (Left(err), next)  => err.reason match {
              case ErrorReason.NotFound   => next.execute(input.cclone, nextFilters, values)
              case ErrorReason.BadRequest => Left(err)
            }
          }
          */
        }
      }

    }

    def to[O, N <: Nat](completion: L => O)(implicit len: Length.Aux[L, N], nat: ToInt[N], as: AsResponse[O, FinalOut]): Route[I,FinalOut] = {
      val slist = builder.buildRouteExecutor
      new Route[I,FinalOut] {
        final val vsetSize = nat()

        final def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[FinalOut] = slist.executeParsers(input, values) match {
          case Right(_) => Right (
            if (collectedFilters.isEmpty && slist.filters.isEmpty) {
              as(completion(builder.build(values)))
            } else {
              slist.executeFilters(input, collectedFilters, values) flatMap { _ => 
                as(completion(builder.build(values)))
              }
            }
          )
          case Left(err) => Left(err)
        }
      }
    }

  }
}

