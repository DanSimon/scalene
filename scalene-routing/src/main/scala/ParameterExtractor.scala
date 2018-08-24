package scalene.routing

import scalene.http._
import scala.annotation.implicitNotFound

import scalene.corerouting._

trait ParameterExtractor[T] {
  def extract(request: RequestContext, key: String) : Result[T]
}
object ParameterExtractor {
  implicit def single[T](implicit formatter: Formatter[T]) = new ParameterExtractor[T] {
    def extract(request: RequestContext, key: String): Result[T] = {
      request.request.parameters.firstValue(key) match {
        case Some(value) => formatter.format(value)
        case None => Left(ParseError.badRequest(s"missing required parameter $key"))
      }
    }
  }

  implicit def seq[T](implicit formatter: Formatter[T]) = new ParameterExtractor[Seq[T]] {
    def extract(request: RequestContext, key: String): Result[Seq[T]] = {
      request.request.parameters.allValues(key).foldLeft[Result[List[T]]](Right(Nil)){case (build, next) => for {
        buildSeq  <- build
        nextRes   <- formatter.format(next)
      } yield nextRes :: buildSeq }
    }
  }

  implicit def option[T](implicit formatter: Formatter[T]) = new ParameterExtractor[Option[T]] {
    def extract(request: RequestContext, key: String): Result[Option[T]] = {
      request.request.parameters.firstValue(key) match {
        case Some(p) => formatter.format(p).map{Some(_)}
        case None => Right(None)
      }
    }
  }

  def literal[T : Formatter](lit :T) = new ParameterExtractor[Unit] {
    val inner = single[T]
    def extract(request: RequestContext, key: String): Result[Unit] = inner
      .extract(request, key) match {
        case Right(res) => if (res == lit) Right(()) else Left(ParseError.badRequest("bad value"))
        case Left(o) => Left(o)
      }
  }

}

@implicitNotFound("Need a Formatter[${X}] in scope")
trait ParameterExtractorProvider[X] {
  type Out
  def provide(extraction: X): ParameterExtractor[Out]
}


object ParameterExtractorProvider {

  implicit def realliteralProvider[T](implicit formatter: Formatter[T]) = new ParameterExtractorProvider[T] {
    type Out = Unit
    def provide(extraction: T): ParameterExtractor[Unit] = ParameterExtractor.literal[T](extraction)
  }

  implicit def extractProvider[T](implicit extractor: ParameterExtractor[T]) = new ParameterExtractorProvider[Extraction[T, T]] {
    type Out = T
    def provide(extraction: Extraction[T, T]): ParameterExtractor[T] = extractor
  }

}


trait ParameterExtractorCreator{
  def apply[X] 
  (key: String, extraction: X)
  (implicit provider: ParameterExtractorProvider[X]) : Parser[RequestContext, provider.Out] = new Parser[RequestContext, provider.Out] {
    val extractor = provider.provide(extraction)
    def parse(request: RequestContext) = extractor.extract(request, key)
  }
}

object Parameter extends ParameterExtractorCreator
object ? extends ParameterExtractorCreator
object & extends ParameterExtractorCreator
