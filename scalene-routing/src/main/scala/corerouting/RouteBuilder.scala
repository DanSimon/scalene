package scalene.corerouting

import scalene.{Async, defer, Deferred}

trait RouteBuilding[I <: Clonable[I], FinalOut] { self: RoutingSuite[I,FinalOut] => 

trait Route[II,O] {
  
  def vsetSize: Int

  def execute(input: II, collectedFilters: List[WrappedFilter[II]], values: VSet) : Result[Deferred[O]]

  final def apply(input: II): Result[Deferred[O]] = execute(input, Nil, if (vsetSize == 0) VSet.empty else VSet(vsetSize))

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


    def executeFilters(input: I, prevFilters: List[WrappedFilter[I]], values: VSet): Deferred[Unit] = {
      (prevFilters ++ filters).foldLeft[Deferred[Unit]](defer{ _ => Async.successful(())}){case (build, next) =>
        build.flatMap {_ =>
          next.parse(input, values)
        }
      }
    }
  }

  abstract class ConstructedRoute[O](val executor: RouteExecutor) {
    def buildResult(values: VSet): O
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
   * constructs the output HList.
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

        val (executor, cell) = next match {
          case p @ CellParser(_) => {
            val (wrapped, cell) = p.wrapped(index)
            (RouteExecutor(prevEx.parsers :+ wrapped, prevEx.filters), cell)
          }
          case f @ CellFilter(_) => {
            val (wrapped, cell) = f.wrapped(index)
            (RouteExecutor(prevEx.parsers, prevEx.filters :+ wrapped), cell)
          }
          case CellPhantom(cell) => {
            (RouteExecutor(prevEx.parsers, prevEx.filters), cell)
          }
        }
        new ConstructedRoute[fuse.Out](executor) {
          def buildResult(values: VSet) = fuse.fuse(prevRoute.buildResult(values), cell.get(values))
          def shallowClone = cons(prevRoute.shallowClone, CellPhantom(cell))
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

    def apply(_routes: Route[I, FinalOut]*): Route[I, FinalOut] = {
      //RouteBuilder.RNil.subroutes(routes.map{route => (n: RouteBuilder[Unit]) => route}: _*)
      val notFoundError: Result[Deferred[FinalOut]] = Left(ParseError.notFound("no route"))
      val routes = _routes.toArray
      new Route[I,FinalOut] {
        val vsetSize = routes.map{_.vsetSize}.max

        def document = ???

        def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : Result[Deferred[FinalOut]] = {
          //at this point we know parsing is successful up to the subroute branching, now find the correct subroute(if any)
          var res = notFoundError
          var i = 0
          var nextInput = input
          while (i < routes.length) {
            val next = routes(i).execute(nextInput, collectedFilters, values)
            next match {
              case Left(err) => err.reason match {
                case ErrorReason.NotFound   => {
                  nextInput = input.cclone
                  i += 1
                }
                case ErrorReason.BadRequest => {
                  res = Left(err)
                  i += routes.length
                }
              }
              case success => {
                res = success
                i += routes.length
              }
            }
          }
          res
        }

      }

    }

  }
}







