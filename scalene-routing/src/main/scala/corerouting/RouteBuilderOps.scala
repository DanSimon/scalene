package scalene.corerouting

import scala.annotation.implicitNotFound
import scala.language.higherKinds

import scalene.Deferred

trait RouteBuilderOpsContainer[I <: Clonable[I], FinalOut] { self: RoutingSuite[I, FinalOut] =>

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
    implicit def comCom[A, B , CA , CB](
      implicit 
      fuse: Fuse[A,B],
      asA: AsCellComponent[I, A, CA],
      asB: AsCellComponent[I, B, CB]
    ) = new RouteBuilderCombiner[CA, CB] {
      type Out = RouteBuilder[fuse.Out]
      def apply(a: CA, b: CB): RouteBuilder[fuse.Out] = {
        val x: CellComponent[I,A] = asA(a)
        val f: RouteBuilder[A] = RouteBuilder.one(x)
        RouteBuilder.cons(f, asB(b))
      }
    }

    /**
     * Combine a buider with a thing that can become a cell component
     */
    implicit def builderCom[A , B , CB, FOut](
      implicit fuse: Fuse.Aux[A,B, FOut],
      as: AsCellComponent[I, B, CB]
    ) = new RouteBuilderCombiner[RouteBuilder[A], CB] {
      type Out = RouteBuilder[FOut]
      def apply(a: RouteBuilder[A], b: CB): RouteBuilder[FOut] = RouteBuilder.cons(a, as(b))
    }

    implicit def fuseRoutes[A,B](implicit fuse: Fuse[A,B]) = new RouteBuilderCombiner[RouteBuilder[A], RouteBuilder[B]] {
      type Out = RouteBuilder[fuse.Out]
      def apply(a: RouteBuilder[A], b: RouteBuilder[B]): RouteBuilder[fuse.Out] = RouteBuilder.fused(a,b)
    }

  }

  object RouteBuilderCombiner extends LowPriorityRouteBuilderCombiners  {
    type Aux[A, B, O] = RouteBuilderCombiner[A,B] { type Out = O }
  }

  implicit class CombineTo[A](val a: A) {
    def +[B](b: B)(implicit com: RouteBuilderCombiner[A,B]): com.Out = com(a,b)
    def and[B](b: B)(implicit com: RouteBuilderCombiner[A,B]): com.Out = com(a,b)
  }

  implicit class AsRouteBuilderOps[P, O](p: P)(implicit as: AsRouteBuilder.Aux[P, O])
  extends RouteBuilderOps[O](as(p))

  implicit class RouteBuilderOps[L](builder: RouteBuilder[L]) {

    def subroutes(subs: (RouteBuilder[L] => Route[I,FinalOut])*): Route[I,FinalOut] = {
      val built = builder.build(0)
      val subroutes: Array[Route[I,FinalOut]] = subs.map{sub => sub(built.shallowClone)}.toArray
      val finalSubroutes = Routes(subroutes : _*)
      new Route[I,FinalOut] {
        val vsetSize = finalSubroutes.vsetSize + builder.size

        def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : Result[Deferred[FinalOut]] = {
          built.executor.executeParsers(input, values) flatMap {unit =>
            val nextFilters = collectedFilters ++ built.executor.filters
            finalSubroutes.execute(input, nextFilters, values)
          }
        }
        def document: DocTreeNode = DocTreeNode(value = built.document(EmptyDoc), children = subroutes.map{_.document}.toList)
      }

    }

    def to[T](completion: L => T)(implicit as: AsResponse[T, FinalOut]): Route[I,FinalOut] = {
      val built = builder.build(0)
      new Route[I,FinalOut] {
        final val vsetSize = builder.size

        val ex = built.executor

        final def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : Result[Deferred[FinalOut]] = ex.executeParsers(input, values) match {
          case Right(_) => Right (
            if (collectedFilters.isEmpty && ex.filters.isEmpty) {
              as(completion(built.buildResult(values)))
            } else {
              ex.executeFilters(input, collectedFilters, values) flatMap { _ => 
                as(completion(built.buildResult(values)))
              }
            }
          )
          case Left(err) => Left(err)
        }

        def document: DocTreeNode = DocTreeNode(built.document(EmptyDoc), Nil)
      }
    }
    def as(const: FinalOut): Route[I,FinalOut] = {
      to(_ => Deferred.successful(const))
    }

    def as(const: Deferred[FinalOut]): Route[I,FinalOut] = {
      to(_ => const)
    }

    def map[U](f: L => U): RouteBuilder[U] = RouteBuilder.mapped[L, U](builder, f)

  }

}

