package scalene

import scalene.http.{Method => HttpMethod, _}
import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success, Failure}

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._

package object routing {


  type Result[T] = Either[ParseError, T]

  type RequestParser[O] = Parser[RequestContext, O]

  type RouteResult[O] = Result[Deferred[O]]
  type BuiltRoute[I,O] = I => RouteResult[O]

  object RequestFilter {
    def apply[O](f: RequestContext => Deferred[O]): Filter[RequestContext,O] = Filter(f)
  }

  object RequestParser {
    def apply[O](p: HttpRequest => Result[O]) = new Parser[RequestContext, O] {
      def parse(r: RequestContext) = p(r.request)
    }

  }

  def ![T]: Extraction[T,T] = IdentityExtraction[T]()

}


package object httprouting 
extends routing.RouteBuilderOps[routing.RequestContext, HttpResponse] 
with routing.RouteBuilding[routing.RequestContext, HttpResponse]
with routing.PathParsing {

  import routing._

  type HttpRoute = Route[RequestContext, HttpResponse]

  //default formatters

  implicit val stringFormatter: Formatter[String] = StringF
  implicit val intF : Formatter[Int] = IntF
  implicit val boolF : Formatter[Boolean] = BooleanF

  //parser tokens




  /**
   * wildcard Accept-all path parser, should be used at the end of a path.
   */
  /*
  val * : PathParser[HNil] = new PathParser[HNil] {
    def parse(components: PathIn) = Right(HNil)
  }
  */

  /**
   * Similar to the `*` parser but also captures the remainder as a string
   */
  /*
  val !* : PathParser[String] = new PathParser[String] {
    def parse(components: PathIn) = Right(components.mkString("/"))
  }
  */

  val GET = Method(HttpMethod.Get)
  val POST = Method(HttpMethod.Post)


  //re-export some stuff from core and http
  type Settings = HttpServerSettings
  val Settings = HttpServerSettings

}
