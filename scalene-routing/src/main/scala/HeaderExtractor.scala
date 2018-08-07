package router

import scalene.http._
import scala.annotation.implicitNotFound

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._

trait HeaderExtractor[T] {
  def extract(request: HttpRequest, key: String) : Result[T]
}
object HeaderExtractor {
  implicit def single[T](implicit formatter: Formatter[T]) = new HeaderExtractor[T] {
    def extract(request: HttpRequest, key: String): Result[T] = {
      request.headerValue(key) match {
        case Some(value) => formatter.format(value)
        case None => Left(ParseError.badRequest(s"missing required parameter $key"))
      }
    }
  }

  implicit def seq[T](implicit formatter: Formatter[T]) = new HeaderExtractor[Seq[T]] {
    def extract(request: HttpRequest, key: String): Result[Seq[T]] = {
      request.headerValues(key).foldLeft[Result[List[T]]](Right(Nil)){case (build, next) => for {
        buildSeq  <- build
        nextRes   <- formatter.format(next)
      } yield nextRes :: buildSeq }
    }
  }

  implicit def option[T](implicit formatter: Formatter[T]) = new HeaderExtractor[Option[T]] {
    def extract(request: HttpRequest, key: String): Result[Option[T]] = {
      request.headerValue(key) match {
        case Some(p) => formatter.format(p).map{Some(_)}
        case None => Right(None)
      }
    }
  }

  def literal[T : Formatter](lit :T) = new HeaderExtractor[HNil] {
    val inner = single[T]
    def extract(request: HttpRequest, key: String): Result[HNil] = inner
      .extract(request, key) match {
        case Right(res) => if (res == lit) Right(HNil) else Left(ParseError.badRequest("bad value"))
        case Left(o) => Left(o)
      }
  }

}

@implicitNotFound("Need a Formatter[${X}] in scope")
trait HeaderExtractorProvider[X] {
  type Out
  def provide(extraction: X): HeaderExtractor[Out]
}


object HeaderExtractorProvider {

  implicit def realliteralProvider[T](implicit formatter: Formatter[T]) = new HeaderExtractorProvider[T] {
    type Out = HNil
    def provide(extraction: T): HeaderExtractor[HNil] = HeaderExtractor.literal[T](extraction)
  }

  implicit def extractProvider[T](implicit extractor: HeaderExtractor[T]) = new HeaderExtractorProvider[Extract[T]] {
    type Out = T
    def provide(extraction: Extract[T]): HeaderExtractor[T] = extractor
  }

}


object Header{
  def apply[X] 
  (key: String, extraction: X)
  (implicit provider: HeaderExtractorProvider[X]) = new Parser[HttpRequest, provider.Out] {
    val extractor = provider.provide(extraction)
    def parse(request: HttpRequest) = extractor.extract(request, key)
  }
}

