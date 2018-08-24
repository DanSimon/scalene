package scalene

import scalene.http.{Method => HttpMethod, _}
import scalene.corerouting._

package object routing extends RoutingSuite[RequestContext, HttpResponse]
with PathParsing {

  type HttpRoute = Route[RequestContext, HttpResponse]

  //default formatters

  implicit val stringFormatter: Formatter[String] = StringF
  implicit val intF : Formatter[Int] = IntF
  implicit val boolF : Formatter[Boolean] = BooleanF

  //parser tokens


  object RequestFilter {
    def apply[O](f: RequestContext => Deferred[O]): Filter[RequestContext,O] = Filter(f)
  }

  object RequestParser {
    def apply[O](p: HttpRequest => Result[O]) = new Parser[RequestContext, O] {
      def parse(r: RequestContext) = p(r.request)
    }

  }

  def ![T]: Extraction[T,T] = IdentityExtraction[T]()



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
