package router
import scalene.http._
import scala.concurrent.{ExecutionContext, Future}

/*

case class Response(code: Code, body: Option[Body]) {
  def future: Future[Response] = Future.successful(this)

  def toHttpResponse(req: HttpRequest): HttpResponse = req.respond(code, body.map{_.body}.getOrElse(HttpBody.NoBody))
}

case class TypedHttpBody(contentType: String, body: HttpBody)

trait BodyEncoder[T] {
  def apply(item: T): Option[TypedHttpBody]
}

object BodyEncoder {

  implicit val StringEncoder = new BodyEncoder[String] {
    def apply(s: String) = Some(TypedHttpBody("text/plain", HttpBody(s)))
  }

  implicit val ident = new BodyEncoder[TypedHttpBody] {
    def apply(b: TypedHttpBody) = Some(b)
  }

}

object ResponseOp {

  implicit class AsResponseOp[T](val item: T) extends AnyVal {

    def asResponse(code: HttpCode)(implicit encoder: BodyEncoder[T]): Response = Response(code, encoder(item))

    def ok(implicit encoder: BodyEncoder[T]) = asResponse(HttpCodes.OK)
  }

  //implicit class AsFutureResponseOp[T](f

}
*/
