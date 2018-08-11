package scalene.routing

import scalene._
import http.{Method => HttpMethod, _}
import scala.annotation.implicitNotFound
import java.util.Arrays

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._


trait PathParser[T] extends Parser[RequestContext, T]

case class ConstantPrefixPath(prefixPieces: List[String]) extends PathParser[HNil] {
  val pieces = prefixPieces.flatMap{_.split("/")}.filter{_ != ""}
  val prefix = "/" + pieces.mkString("/")
  
  def size = pieces.size

  def add(segment: String) = copy(prefixPieces = this.prefixPieces :+ segment)

  def parse(ctx: RequestContext): Result[HNil] = ???

}

class ExtractionSegmentParser[T](formatter: Parser[Raw,T]) extends PathParser[T] {
  def parse(components: RequestContext): Result[T] = if (components.hasNext) {
    formatter.parse(components.next).left.map{e => e.copy(reason = ErrorReason.NotFound)}
  } else {
    Left(ParseError.notFound("expected component"))
  }
}

case class ExactMatchPath(method: HttpMethod, prefix: ConstantPrefixPath) extends Parser[RequestContext, HNil] {
  val bytes = s"${method.name.toUpperCase} ${prefix.prefix}".getBytes
  val ok = Right(HNil)
  def parse(req: RequestContext): Result[HNil] = {
    if (Arrays.equals(req.request.firstLine, 0, bytes.length, bytes, 0, bytes.length)) {
      req.pathIterator.advance(prefix.size)
      ok
    } else {
      Left(ParseError.notFound("Not a match"))
    }
  }
}

//mixed into package object
trait PathParsing {

  implicit class PathCombine[A](val a: A) {
    def /[B](b: B)(implicit com: RouteBuilderCombiner[A,B]): com.Out = com(a,b)
  }

  implicit def combineStringString = new RouteBuilderCombiner[String, String] {
    type Out = ConstantPrefixPath
    def apply(a: String, b: String): ConstantPrefixPath = new ConstantPrefixPath(a :: b :: Nil)
  }

  implicit def combinePrefixString = new RouteBuilderCombiner[ConstantPrefixPath, String] {
    type Out = ConstantPrefixPath
    def apply(a: ConstantPrefixPath, b: String): ConstantPrefixPath = new ConstantPrefixPath(a.pieces :+ b)
  }

  implicit def combineExactMatchString = new RouteBuilderCombiner[ExactMatchPath, String] {
    type Out = ExactMatchPath
    def apply(a: ExactMatchPath, b: String): ExactMatchPath = a.copy(prefix = a.prefix.add(b))
  }

  implicit def combineMethodString = new RouteBuilderCombiner[Method, String] {
    type Out = ExactMatchPath
    def apply(a: Method, b: String) = ExactMatchPath(a.method, ConstantPrefixPath(b :: Nil))
  }

  implicit def combineParserExtraction[A, B](implicit 
    comb: RouteBuilderCombiner[PathParser[A],ExtractionSegmentParser[B]],
    formatter: Parser[Raw, B]
  ) = new RouteBuilderCombiner[PathParser[A], Extract[B]] {
    type Out = comb.Out
    def apply(a: PathParser[A], b: Extract[B]): Out = {
      val extractor = new ExtractionSegmentParser[B](formatter)
      comb(a, extractor)
    }
  }

  implicit def combineStringExtraction[A](implicit
    comb: RouteBuilderCombiner[PathParser[HNil], ExtractionSegmentParser[A]],
    formatter: Parser[Raw, A]
  ) = new RouteBuilderCombiner[String, Extract[A]] {
    type Out = comb.Out
    def apply(a: String, b: Extract[A]): Out = comb((new ConstantPrefixPath(a :: Nil)) : PathParser[HNil], new ExtractionSegmentParser(formatter))
  }

}

