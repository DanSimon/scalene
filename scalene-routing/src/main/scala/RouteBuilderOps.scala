package scalene.routing

import scala.annotation.implicitNotFound
import scala.language.higherKinds

import scalene._

trait RouteBuilderOpsContainer[I <: Clonable[I], FinalOut] { self: RouteBuilding[I, FinalOut] =>

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

  object RouteBuilderCombiner extends LowPriorityRouteBuilderCombiners 

  implicit class CombineTo[A](val a: A) {
    def +[B](b: B)(implicit com: RouteBuilderCombiner[A,B]): com.Out = com(a,b)
  }

  implicit class AsRouteBuilderOps[O, P[_,_]](p: P[I,O])(implicit as: AsRouteBuilder[O, P])
  extends RouteBuilderOps[O](as(p))

  implicit class RouteBuilderOps[L](builder: RouteBuilder[L]) {

    def subroutes(subs: (RouteBuilder[L] => Route[I,FinalOut])*): Route[I,FinalOut] = {
      val slist = builder.buildRouteExecutor
      val subroutes: Array[Route[I,FinalOut]] = subs.map{sub => sub(builder.shallowClone)}.toArray
      val notFoundError: RouteResult[FinalOut] = Left(ParseError.notFound("no route"))
      new Route[I,FinalOut] {
        val vsetSize = subroutes.map{_.vsetSize}.max + builder.size

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

    def to(completion: L => Deferred[FinalOut]): Route[I,FinalOut] = {
      val slist = builder.buildRouteExecutor
      new Route[I,FinalOut] {
        final val vsetSize = slist.size

        final def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[FinalOut] = slist.executeParsers(input, values) match {
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
    def as(const: FinalOut): Route[I,FinalOut] = {
      to(_ => Deferred.successful(const))
    }

    def as(const: Deferred[FinalOut]): Route[I,FinalOut] = {
      to(_ => const)
    }

  }

}

