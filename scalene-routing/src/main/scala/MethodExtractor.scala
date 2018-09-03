package scalene.routing

import scalene.http.{Method => HttpMethod}
import scala.annotation.implicitNotFound

import scalene.corerouting._

case class Method(method: HttpMethod) extends Parser[RequestContext, Unit] {
  def parse(req: RequestContext) = if (req.request.method == method) {
    Right(())
  } else {
    Left(ParseError.notFound(s"Expected method $method, got ${req.request.method}"))
  }
  override def document(d: DocType): DocType = d.copy(method = method.name.toString)
}

object Method {

  /*
  def apply(method: HttpMethod) = new Parser[RequestContext, HNil] {
    def parse(req: RequestContext) = if (req.request.method == method) {
      Right(HNil)
    } else {
      Left(ParseError.notFound(s"Expected method $method, got ${req.request.method}"))
    }
  }
  */

  val ! = new Parser[RequestContext, HttpMethod] {
    def parse(req: RequestContext) = Right(req.request.method)
  }

}
