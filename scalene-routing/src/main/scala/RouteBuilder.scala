package scalene.routing

import scalene.{Async, defer, Deferred}

trait RouteBuilding[I <: Clonable[I], FinalOut] { self: RouteBuilderOpsContainer[I,FinalOut] => 

  /*
   * A RouteExecutor represents a fully constructed route parser.  It handles the actual
   * execution of the routes parsers and filters, accumulating the extracted
   * values into a VSet.  A corrosponding RouteBuilder handles properly typing the
   * extracted values into a result HList
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

  /**
   * This typeclass is used with AsCellComponent to turn a parser or a filter into a RouteBuilder 
   */
  trait AsRouteBuilder[O, P] {
    def apply(p: P): RouteBuilder[O]
  }

  object AsRouteBuilder {

    implicit def liftAsCellComponent[O, P[_,_]](implicit 
      as: AsCellComponent[P]
    ) = new AsRouteBuilder[O, P[I,O]] {
      def apply(p: P[I,O]): RouteBuilder[O] = RouteBuilder.one(as(p))
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

    def buildRouteExecutor : RouteExecutor

    def build(values: VSet): L

    def size: Int

    def shallowClone: RouteBuilder[L]

  }
  object RouteBuilder {

    object RNil extends RouteBuilder[Unit] {
      
      def buildRouteExecutor = RouteExecutor(Nil, Nil)
      def build(values: VSet) = ()

      val size = 0
      def shallowClone: RouteBuilder[Unit] = this
    }

    def one[L](com: CellComponent[I,L]) = cons[L, Unit](RNil, com)(Fuse.uFusev)

    def cons[O, L](prev : RouteBuilder[L], next: CellComponent[I,O])(implicit fuse: Fuse[L, O]): RouteBuilder[fuse.Out] = new RouteBuilder[fuse.Out] {

      val size = prev.size + 1
      val prevList = prev.buildRouteExecutor

      val (slist, cell) = next match {
        case p @ CellParser(_) => {
          val (wrapped, cell) = p.wrapped(size - 1)
          (RouteExecutor(prevList.parsers :+ wrapped, prevList.filters), cell)
        }
        case f @ CellFilter(_) => {
          val (wrapped, cell) = f.wrapped(size - 1)
          (RouteExecutor(prevList.parsers, prevList.filters :+ wrapped), cell)
        }
        case CellPhantom(cell) => {
          (RouteExecutor(prevList.parsers, prevList.filters), cell)
        }
      }
      val buildRouteExecutor = slist
      def build(values: VSet): fuse.Out = fuse.fuse(prev.build(values), cell.get(values))

      def shallowClone = cons(prev.shallowClone, CellPhantom(cell))

    }

    def mapped[T, U](nested: RouteBuilder[T], map: T => U): RouteBuilder[U] = new RouteBuilder[U] {

      val size = nested.size 
      def buildRouteExecutor = nested.buildRouteExecutor

      def build(values: VSet) = map(nested.build(values))

      def shallowClone = mapped(nested.shallowClone, map) //is this right?!?!?!
    }
  }


  object Routes {

    def apply(_routes: Route[I, FinalOut]*): Route[I, FinalOut] = {
      //RouteBuilder.RNil.subroutes(routes.map{route => (n: RouteBuilder[Unit]) => route}: _*)
      val notFoundError: RouteResult[FinalOut] = Left(ParseError.notFound("no route"))
      val routes = _routes.toArray
      new Route[I,FinalOut] {
        val vsetSize = routes.map{_.vsetSize}.max

        def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[FinalOut] = {
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







