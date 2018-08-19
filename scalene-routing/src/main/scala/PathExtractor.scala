package scalene.routing

import scalene._
import http.{Method => HttpMethod, _}
import scala.annotation.implicitNotFound
import java.util.Arrays

case class ConstantPrefixPath(prefixPieces: List[String]) extends Parser[RequestContext, Unit] {
  val pieces = prefixPieces.flatMap{_.split("/")}.filter{_ != ""}
  val prefix = "/" + pieces.mkString("/")
  
  def size = pieces.size

  def add(segment: String) = copy(prefixPieces = this.prefixPieces :+ segment)

  def parse(ctx: RequestContext): Result[Unit] = ???

}

class ExtractionSegmentParser[T](formatter: Parser[String,T]) extends Parser[RequestContext, T] {
  def parse(components: RequestContext): Result[T] = if (components.hasNext) {
    formatter.parse(components.next).left.map{e => e.copy(reason = ErrorReason.NotFound)}
  } else {
    Left(ParseError.notFound("expected component"))
  }
}

case class ExactMatchPath(method: HttpMethod, prefix: ConstantPrefixPath) extends Parser[RequestContext, Unit] {
  val bytes = s"${method.name.toUpperCase} ${prefix.prefix}".getBytes
  val ok = Right(())
  def parse(req: RequestContext): Result[Unit] = {
    if (Arrays.equals(req.request.firstLine, 0, bytes.length, bytes, 0, bytes.length)) {
      req.pathIterator.advance(prefix.size)
      ok
    } else {
      Left(ParseError.notFound("Not a match"))
    }
  }
}

trait AsPathParser[T] {
  type Out
  def apply(in : T): Parser[RequestContext, Out]
}

object AsPathParser {

  type Aux[A,B] = AsPathParser[A]{type Out = B}

  implicit def LiteralParser = new AsPathParser[String] {
    type Out = Unit
    def apply(in: String) = ConstantPrefixPath(in :: Nil)
  }

  implicit def extractionParser[A,B](implicit formatter: Parser[String, A]) = new AsPathParser[Extraction[A,B]] {
    type Out = B
    def apply(in: Extraction[A,B]) = in.extraction(new ExtractionSegmentParser(formatter))
  }

  implicit def identity[T, P <: Parser[RequestContext, T]] = new AsPathParser[P] {
    type Out = T
    def apply(p: P) = p
  }

  

  implicit val p = identity[Unit, ExactMatchPath]
  implicit val q = identity[Unit, Method]

}

trait LowPriorityPathParsing { self: RouteBuilding[RequestContext, HttpResponse] with routing.RouteBuilderOps[RequestContext, HttpResponse] =>

  implicit def combineTwoThings[A, B, AOut, BOut](implicit 
    asA: AsPathParser.Aux[A, AOut],
    asB: AsPathParser.Aux[B, BOut],
    comb: RouteBuilderCombiner[Parser[RequestContext, AOut], Parser[RequestContext, BOut]]
  ) = new RouteBuilderCombiner[A, B] {
    type Out = comb.Out
    def apply(a: A, b: B): Out = comb(asA(a), asB(b))
  }

  implicit def extendRouteBuilder[L, A, AOut](implicit
    asA: AsPathParser.Aux[A, AOut],
    comb: RouteBuilderCombiner[RouteBuilder[L], Parser[RequestContext, AOut]]
  ) = new RouteBuilderCombiner[RouteBuilder[L], A] {
    type Out = comb.Out
    def apply(builder: RouteBuilder[L], a: A): Out = comb(builder, asA(a))
  }

}

//mixed into package object
trait PathParsing extends LowPriorityPathParsing { self: RouteBuilding[RequestContext, HttpResponse] with routing.RouteBuilderOps[RequestContext, HttpResponse] =>

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

}
