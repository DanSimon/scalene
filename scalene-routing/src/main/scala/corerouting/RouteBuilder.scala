package scalene.corerouting

import scalene.{Async, defer, Deferred}

trait RouteBuilding[I <: Clonable[I], FinalOut] { self: RoutingSuite[I,FinalOut] => 

trait Route[II,O] {
  
  def vsetSize: Int

  def execute(input: II, values: VSet) : Result[Deferred[O]]

  private final val myvset = if (vsetSize == 0) VSet.empty else VSet(vsetSize)

  final def apply(input: II): Result[Deferred[O]] = execute(input, myvset)

  def document: DocTreeNode

}

  /*
   * A RouteExecutor represents a fully constructed route parser.  It handles the actual
   * execution of the routes parsers and filters, accumulating the extracted
   * values into a VSet.  
   */
  case class RouteExecutor(parsers: List[WrappedParser[I]], filters: List[WrappedFilter[I]]) {
    def size = parsers.size + filters.size

    private val unitRight = Right(())
    private val arr = parsers.toArray

    def executeParsers(input: I, values: VSet): Result[Unit] = {
      var res: Result[Unit] = unitRight
      var i = 0
      while (i < arr.length && res.isRight) {
        res = arr(i).parse(input, values)
        i += 1
      }
      res
    }


    def executeFilters(input: I, values: VSet): Deferred[Unit] = {
      filters.foldLeft[Deferred[Unit]](defer{ _ => Async.successful(())}){case (build, next) =>
        build.flatMap {_ =>
          next.parse(input, values)
        }
      }
    }
  }

  abstract class ConstructedRoute[O](val executor: RouteExecutor) {
    def buildResult(values: VSet): O

    /**
     * shallow clones are used for subrouting.  It prevents running the
     * parsers/filters of the root of a tree of subroutes for every subroute
     */
    def shallowClone: RouteBuilder[O]

    def document(builder: DocType): DocType
  }



  /**
   * This typeclass is used with AsCellComponent to turn a parser or a filter into a RouteBuilder 
   */
  trait AsRouteBuilder[P] {
    type Out
    def apply(p: P): RouteBuilder[Out]
  }

  object AsRouteBuilder {

    type Aux[P, O] = AsRouteBuilder[P] { type Out = O }

    implicit def liftAsCellComponent[O, P](implicit 
      as: AsCellComponent[I, O, P]
    ) = new AsRouteBuilder[P] {
      type Out = O
      def apply(p: P): RouteBuilder[O] = RouteBuilder.one(as(p))
    }

  }


  /**
   * A RouteBuilder has three responsibilities.  First, it stores all the type
   * information of a route, aka the composite type of extracted values.  Second, it
   * builds the RouteExecutor which handles the actual execution of the parsers/filters.
   * Lastly, it takes the VSet of extracted values produced by its RouteExecutor and
   * constructs the output type.
   */
  trait RouteBuilder[L] {

    def build(offset: Int) : ConstructedRoute[L]

    def size: Int

  }
  object RouteBuilder {

    object RNil extends RouteBuilder[Unit] {
      
      def build(offset: Int) = new ConstructedRoute[Unit](RouteExecutor(Nil, Nil)) {
        def buildResult(values: VSet) = ()
        def shallowClone = RNil
        def document(builder: DocType): DocType = builder
      }

      val size = 0
      def shallowClone: RouteBuilder[Unit] = this
    }

    def one[L](com: CellComponent[I,L]) = cons[L, Unit](RNil, com)(Fuse.uFusev)

    def cons[O, L](prev : RouteBuilder[L], next: CellComponent[I,O])(implicit fuse: Fuse[L, O]): RouteBuilder[fuse.Out] = new RouteBuilder[fuse.Out] {

      val size = prev.size + 1

      def build(offset: Int): ConstructedRoute[fuse.Out] = {
        val prevRoute = prev.build(offset)
        val prevEx = prevRoute.executor
        val index = offset + size - 1

        //shallow clones are only needed for parsers, since only parsers can
        //appear multiple times when parsing a route (if the first branch in a
        //subroute tree fails), but a filter failure always fails the whole
        //request so there's no need to shallow clone them
        val (executor, cell, shallowClonedCell: CellComponent[I,O]) = next match {
          case p @ CellParser(_) => {
            val (wrapped, cell) = p.wrapped(index)
            (RouteExecutor(prevEx.parsers :+ wrapped, prevEx.filters), cell, CellPhantom(cell))
          }
          case f @ CellFilter(_) => {
            val (wrapped, cell) = f.wrapped(index)
            (RouteExecutor(prevEx.parsers, prevEx.filters :+ wrapped), cell, f)
          }
          case c @ CellPhantom(cell) => {
            (RouteExecutor(prevEx.parsers, prevEx.filters), cell, CellPhantom(cell))
          }
        }
        new ConstructedRoute[fuse.Out](executor) {
          def buildResult(values: VSet) = fuse.fuse(prevRoute.buildResult(values), values.get(cell))
          def shallowClone = cons(prevRoute.shallowClone, shallowClonedCell)
          def document(builder: DocType): DocType = next.document(prevRoute.document(builder))
        }

      }


    }

    def mapped[T, U](nested: RouteBuilder[T], map: T => U): RouteBuilder[U] = new RouteBuilder[U] {

      val size = nested.size 
      def build(offset: Int) = {
        val n = nested.build(offset)
        new ConstructedRoute[U](n.executor) {
          def buildResult(values: VSet) = map(n.buildResult(values))
          def shallowClone = mapped(n.shallowClone, map) //is this right?!?!?!
          def document(builder: DocType): DocType = n.document(builder)
        }
      }

    }

    def fused[A,B](a: RouteBuilder[A], b: RouteBuilder[B])(implicit fuse: Fuse[A,B]): RouteBuilder[fuse.Out] = new RouteBuilder[fuse.Out] {

      val size = a.size + b.size
      def build(offset: Int) = {
        val builtA = a.build(offset)
        val builtB = b.build(offset + a.size)
        val executor = RouteExecutor(
          builtA.executor.parsers ++ builtB.executor.parsers,
          builtA.executor.filters ++ builtB.executor.filters
        )
        new ConstructedRoute[fuse.Out](executor) {
          def buildResult(values: VSet) = fuse.fuse(builtA.buildResult(values), builtB.buildResult(values))
          def shallowClone = fused(builtA.shallowClone, builtB.shallowClone)
          def document(builder: DocType): DocType = builtB.document(builtA.document(builder))
        }
      }
    }
  }

  object Routes {

    class MultiRoute(_routes: Seq[Route[I, FinalOut]]) extends Route[I, FinalOut] {
      val notFoundError: Result[Deferred[FinalOut]] = Left(ParseError.notFound("no route"))
      val routes = _routes.toArray
      val numRoutes = routes.length
      val vsetSize = routes.map{_.vsetSize}.max

      def document = ???

      def execute(input: I, values: VSet) : Result[Deferred[FinalOut]] = {
        //at this point we know parsing is successful up to the subroute branching, now find the correct subroute(if any)
        var res = notFoundError
        var i = 0
        var nextInput = input
        while (i < numRoutes) {
          val next = routes(i).execute(nextInput, values)
          next match {
            case success @ Right(_) => {
              res = success
              i += numRoutes
            }
            case Left(err) => err.reason match {
              case ErrorReason.NotFound   => {
                nextInput = input.cclone
                i += 1
              }
              case ErrorReason.BadRequest => {
                res = Left(err)
                i += routes.length
              }
              case ErrorReason.Error => {
                res = Left(err)
                i += routes.length
              }
            }
          }
        }
        res
      }

    }

    def apply(_routes: Route[I, FinalOut]*): Route[I, FinalOut] = new MultiRoute(_routes)

  }
}







