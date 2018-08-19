package scalene.routing

import scalene.{Async, defer, Deferred}

trait RouteBuilding[I <: Clonable[I], FinalOut] { self: RouteBuilderOps[I,FinalOut] => 

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
  trait AsRouteBuilder[O, P[_,_]] {
    def apply(p: P[I,O]): RouteBuilder[O]
  }

  object AsRouteBuilder {

    implicit def liftAsCellComponent[O, P[_,_]](implicit 
      as: AsCellComponent[P],
      fuse: Fuse.Aux[Unit, O, O]
    ) = new AsRouteBuilder[O, P] {
      def apply(p: P[I,O]): RouteBuilder[O] = RouteBuilder.cons(RouteBuilder.RNil, as(p))
    }

    type Aux[O, P[_,_], L] = AsRouteBuilder[O, P]{ type FixedOut = L }

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

    def subroutes(subs: (RouteBuilder[L] => Route[I,FinalOut])*): Route[I,FinalOut] = {
      val slist = buildRouteExecutor
      val subroutes: Array[Route[I,FinalOut]] = subs.map{sub => sub(shallowClone)}.toArray
      val notFoundError: RouteResult[FinalOut] = Left(ParseError.notFound("no route"))
      new Route[I,FinalOut] {
        val vsetSize = subroutes.map{_.vsetSize}.max + size

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

    def to[T](completion: L => T)(implicit as: AsResponse[T, FinalOut]): Route[I,FinalOut] = {
      val slist = buildRouteExecutor
      new Route[I,FinalOut] {
        final val vsetSize = slist.size

        final def execute(input: I, collectedFilters: List[WrappedFilter[I]], values: VSet) : RouteResult[FinalOut] = slist.executeParsers(input, values) match {
          case Right(_) => Right (
            if (collectedFilters.isEmpty && slist.filters.isEmpty) {
              as(completion(build(values)))
            } else {
              slist.executeFilters(input, collectedFilters, values) flatMap { _ => 
                as(completion(build(values)))
              }
            }
          )
          case Left(err) => Left(err)
        }
      }
    }
    def to(const: FinalOut): Route[I,FinalOut] = {
      to(_ => Deferred.successful(const))
    }

    def to(const: Deferred[FinalOut]): Route[I,FinalOut] = {
      to(_ => const)
    }
  }
  object RouteBuilder {

    object RNil extends RouteBuilder[Unit] {
      
      def buildRouteExecutor = RouteExecutor(Nil, Nil)
      def build(values: VSet) = ()

      val size = 0
      def shallowClone: RouteBuilder[Unit] = this
    }

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

    /*
    def mapped[O, L <: HList](nested: RouteBuilder[L], map: L => O)(implicit f: Fuse[HNil, O]): RouteBuilder[f.Out] = new RouteBuilder[f.Out] {

      val size = nested.size 
      def buildRouteExecutor = nested.buildRouteExecutor

      def build(values: VSet): f.Out = f.fuse(HNil, map(nested.build(values)))

      def shallowClone = mapped(nested.shallowClone, map) //is this right?!?!?!
    }
    */
  }


  object Routes {

    def apply(routes: Route[I, FinalOut]*): Route[I, FinalOut] = {
      RouteBuilder.RNil.subroutes(routes.map{route => (n: RouteBuilder[Unit]) => route}: _*)
    }

  }
}







