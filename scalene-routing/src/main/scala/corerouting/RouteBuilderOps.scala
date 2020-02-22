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
     *
     * Note - The order of implicit parameters here is really important, putting fuse first causes failures :/
     */
    implicit def comCom[A, B , CA , CB](
      implicit 
      asA: AsCellComponent[I, A, CA],
      asB: AsCellComponent[I, B, CB],
      fuse: Fuse[A,B]
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
     * this doesn't work, appears to be a scala bug :(
    implicit def builderCom[A , B , CB, FOut](
      implicit fuse: Fuse.Aux[A,B, FOut],
      as: AsCellComponent[I, B, CB]
    ) = new RouteBuilderCombiner[RouteBuilder[A], CB] {
      type Out = RouteBuilder[FOut]
      def apply(a: RouteBuilder[A], b: CB): RouteBuilder[FOut] = RouteBuilder.cons(a, as(b))
    }
     */
    /**
     * Combine a buider with a parser
     */
    implicit def builderParser[A , B , FOut](
      implicit fuse: Fuse.Aux[A,B, FOut],
    ) = new RouteBuilderCombiner[RouteBuilder[A], Parser[I,B]] {
      type Out = RouteBuilder[FOut]
      def apply(a: RouteBuilder[A], b: Parser[I,B]): RouteBuilder[FOut] = RouteBuilder.cons(a, CellParser(b))
    }


    /**
     * Combine a buider with a filter
     */
    implicit def builderFilter[A , B , FOut](
      implicit fuse: Fuse.Aux[A,B, FOut],
    ) = new RouteBuilderCombiner[RouteBuilder[A], Filter[I,B]] {
      type Out = RouteBuilder[FOut]
      def apply(a: RouteBuilder[A], b: Filter[I,B]): RouteBuilder[FOut] = RouteBuilder.cons(a, CellFilter(b))
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

    /**
     * Create a tree of subroutes with this RouteBuilder as the root.  Subroute
     * parsing will only begin if the root parser is successful.  However,
     * parser-before-filter execution order is maintained, so if the root
     * RouteBuilder includes a filter, that filter will still not be executed until a subroute is successfully parsed.
     *
     * Subroute parsing is attempted in the order they are given, stopping as soon as one of them succeeds
     */
    def subroutes(subs: (RouteBuilder[L] => Route[I,FinalOut])*): Route[I,FinalOut] = {
      val built = builder.build(0)
      val subroutes: Array[Route[I,FinalOut]] = subs.map{sub => sub(built.shallowClone)}.toArray
      val finalSubroutes = Routes(subroutes : _*)
      new Route[I,FinalOut] {
        val vsetSize = finalSubroutes.vsetSize + builder.size

        def execute(input: I, values: VSet) : Result[Deferred[FinalOut]] = {
          built.executor.executeParsers(input, values) flatMap {unit =>
            finalSubroutes.execute(input, values)
          }
        }
        def document: DocTreeNode = DocTreeNode(value = built.document(EmptyDoc), children = subroutes.map{_.document}.toList)
      }

    }

    /**
     * Combine this RouteBuilder with a function that accepts its output to
     * produce an object of the final Response type.
     */
    def to[T](completion: L => T)(implicit as: AsResponse[T, FinalOut]): Route[I,FinalOut] = {
      new Route[I,FinalOut] {
        val built = builder.build(0)
        final val vsetSize = builder.size

        val ex = built.executor

        final def execute(input: I, values: VSet) : Result[Deferred[FinalOut]] = {
          ex.executeParsers(input, values) match {
            case Right(_) => Right (
              if (ex.filters.isEmpty) {
                as(completion(built.buildResult(values)))
              } else {
                ex.executeFilters(input, values) flatMap { _ => 
                  as(completion(built.buildResult(values)))
                }
              }
            )
            case Left(err) => Left(err)
          }
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

  object NewOps {

    implicit class CompletionJoinerOps[T, O](thing: T)(implicit joiner: CompletionJoiner[T, O]) {
      def nto(completion: O => FinalOut): Route[I, FinalOut] = joiner.complete(thing, completion)
    }

    trait CompletionJoiner[T, O] {
      def complete(builder: T, completion: O => FinalOut): Route[I, FinalOut]
    }

    object CompletionJoiner {
      implicit def routeBuilderSyncJoiner[O] = new CompletionJoiner[RouteBuilder[O], O] {
        def complete(builder: RouteBuilder[O], completion: O => FinalOut): Route[I, FinalOut] = {
          new Route[I,FinalOut] {
            val built = builder.build(0)
            final val vsetSize = builder.size

            val ex = built.executor

            final def execute(input: I, values: VSet) : Result[Deferred[FinalOut]] = {
              ex.executeParsers(input, values) match {
                case Right(_) => Right (
                  if (ex.filters.isEmpty) {
                    Deferred.successful(completion(built.buildResult(values)))
                  } else {
                    ex.executeFilters(input, values) flatMap { _ => 
                      Deferred.successful(completion(built.buildResult(values)))
                    }
                  }
                )
                case Left(err) => Left(err)
              }
            }

            def document: DocTreeNode = DocTreeNode(built.document(EmptyDoc), Nil)
          }
        }
      }
    }

  }

}

