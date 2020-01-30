package scalene

import scalene.http.{Method => HttpMethod, _}
import scalene.corerouting._
import scalene.stream._

package object routing extends RoutingSuite[RequestContext, HttpResponse]
with PathParsing with ResponseBuilding {

  type HttpRoute = Route[RequestContext, HttpResponse]

  final type DocType = ParserDoc
  val EmptyDoc = ParserDoc.empty

  //default formatters

  implicit val stringFormatter: Formatter[String] = StringF
  implicit val intF : Formatter[Int] = IntF
  implicit val boolF : Formatter[Boolean] = BooleanF
  implicit val doubleF = DoubleF
  implicit val longF = LongF


  //parser tokens


  object RequestFilter {
    def apply[O](f: RequestContext => Deferred[O]): Filter[RequestContext,O] = Filter(f)
  }

  object RequestParser {
    def apply[O](p: HttpRequest => Result[O]) = new Parser[RequestContext, O] {
      def parse(r: RequestContext) = p(r.request)
    }

  }

  def ![T]: Extraction[T,T] = ![T]("extraction")
  def ![T](name: String): Extraction[T,T] = IdentityExtraction(name) 

  def optional[A,B](extraction: Extraction[A, B]): Extraction[A, Option[B]] = extraction
    .map{t => Some(t) : Option[B]}
    .recover(_ => None)

  //def seq[T](extraction: Extraction[String, T]
  //

  def Attachment[T](implicit provider: AttachmentProvider[T]): Parser[RequestContext, T] = new Parser[RequestContext, T] {
    def parse(r: RequestContext) = Right(r.routingContext.attachments.getOrCreate(provider))
  }


  val Context: Parser[RequestContext, RequestHandlerContext] = new Parser[RequestContext, RequestHandlerContext] {
    def parse(r: RequestContext) = Right(r.routingContext.requestHandlerContext)
  }

  /**
   * wildcard Accept-all path parser, should be used at the end of a path.
   */
  val * = new Parser[RequestContext, Unit] {
    def parse(req: RequestContext): Result[Unit] = {
      while (req.hasNext) { req.next }
      Right(())
    }
  }

  /**
   * Similar to the `*` parser but also captures the remainder as a string
   */
  val !* = new Parser[RequestContext,String] {
    def parse(req: RequestContext): Result[String] = Right(req.mkString("/"))
  }

  val GET = Method(HttpMethod.Get)
  val POST = Method(HttpMethod.Post)


  //re-export some stuff from core and http
  type Settings = HttpServerSettings
  val Settings = HttpServerSettings

  type Deferred[T] = scalene.Deferred[T]
  val Deferred = scalene.Deferred

  type ServerSettings = scalene.ServerSettings
  val ServerSettings = scalene.ServerSettings

  object BasicConversions {
    trait PlainTextBody[T] extends BodyFormatter[T] {
      def format(obj: T) = BodyData.Static(obj.toString.getBytes())
      def contentType = Some(ContentType.`text/plain`)
    }

    implicit object StringToBody extends PlainTextBody[String] 
    implicit object IntToBody extends PlainTextBody[Int] 
    implicit object FloatToBody extends PlainTextBody[Float] 
    implicit object DoubleToBody extends PlainTextBody[Double] 
    implicit object BooleanToBody extends PlainTextBody[Boolean] 
    implicit def streamBodyFormatter[T](implicit b: PlainTextBody[T]) = new BodyFormatter[Stream[T]] {
      def format(stream: Stream[T]) = BodyData.Stream(stream.map{item => ReadBuffer(b.format(item).data)})
      def contentType = Some(ContentType.`text/plain`)
    }

  }

}
