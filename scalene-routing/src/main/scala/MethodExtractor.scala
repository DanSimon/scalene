package scalene.routing

import scalene.http.{Method => HttpMethod}
import scala.annotation.implicitNotFound

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._

case class Method(method: HttpMethod) extends Parser[RequestContext, Unit] {
  def parse(req: RequestContext) = if (req.request.method == method) {
    Right(())
  } else {
    Left(ParseError.notFound(s"Expected method $method, got ${req.request.method}"))
  }
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
