package scalene.routing

import scalene._
import http.{Method => HttpMethod, _}
import scala.annotation.implicitNotFound
import java.util.Arrays
import scala.reflect.ClassTag
import scalene.corerouting._

case class ConstantPrefixPath(prefixPieces: List[String]) extends Parser[RequestContext, Unit] {
  val pieces = prefixPieces.flatMap{_.split("/")}.filter{_ != ""}
  val prefix = "/" + pieces.mkString("/")
  val bytes = prefix.getBytes
  
  val size = pieces.size

  def add(segment: String) = copy(prefixPieces = this.prefixPieces :+ segment)

  def parse(ctx: RequestContext): Result[Unit] = {
    //println(s"checking ${ctx.request.path} for '$prefix'")
    if (ctx.request.path.startsWith(prefix)) {
      ctx.pathIterator.advance(pieces.size)
      Right(())
    } else {
      Left(ParseError.notFound("Not a match"))
    }
  }

  override def document(p: ParserDoc) = {
    val s = p.pathSegments ++ pieces
    p.copy(pathSegments = s)
  }

}

class ExtractionSegmentParser[T](name: String, formatter: Parser[String,T])(implicit ct: ClassTag[T]) extends Parser[RequestContext, T] {
  val error = Left(ParseError.notFound(s"missing component $name of type ${ct.runtimeClass.getSimpleName}"))
  def parse(components: RequestContext): Result[T] = if (components.hasNext) {
    formatter.parse(components.next).left.map{e => e.copy(reason = ErrorReason.NotFound)}
  } else {
    error
  }

  override def document(p: ParserDoc) = p.withPathSegment(s"<${ct.runtimeClass.getSimpleName}>")

}

case class ExactMatchPath(method: HttpMethod, prefix: ConstantPrefixPath) extends Parser[RequestContext, Unit] {
  val prefixString = s"${method.name.toUpperCase} ${prefix.prefix}"
  val bytes = prefixString.getBytes
  val ok = Right(())
  val error = Left(ParseError.notFound(s"did not match $prefixString"))
  def parse(req: RequestContext): Result[Unit] = {
    if (Arrays.equals(req.request.firstLine, 0, bytes.length, bytes, 0, bytes.length)) {
      req.pathIterator.advance(prefix.size)
      ok
    } else {
      error
    }
  }

  override def document(p: ParserDoc) = prefix.document(p).copy(method = method.name.toString)
}

trait AsPathParser[T, Out] {
  def apply(in : T): Parser[RequestContext, Out]
}

object AsPathParser {

  type Aux[A,B] = AsPathParser[A,B]

  implicit def LiteralParser = new AsPathParser[String, Unit] {
    def apply(in: String) = ConstantPrefixPath(in :: Nil)
  }

  implicit def extractionParser[A,B](implicit formatter: Parser[String, A], ct: ClassTag[A]) = new AsPathParser[Extraction[A,B], B] {
    def apply(in: Extraction[A,B]) = in.extraction(new ExtractionSegmentParser(in.name, formatter))
  }

  implicit def identity[T, P <: Parser[RequestContext, T]] = new AsPathParser[P, T] {
    def apply(p: P) = p
  }

  

  implicit val p = identity[Unit, ExactMatchPath]
  implicit val q = identity[Unit, Method]

}

trait LowPriorityPathParsing { 

  implicit def pathCombineTwoThings[A, B, AOut, BOut](implicit 
    asA: AsPathParser.Aux[A, AOut],
    asB: AsPathParser.Aux[B, BOut],
    fuse: Fuse[AOut,BOut]
  ) = new RouteBuilderCombiner[A, B] {
    type Out = RouteBuilder[fuse.Out]
    def apply(a: A, b: B): Out = RouteBuilder.cons(RouteBuilder.one(CellParser(asA(a))), CellParser(asB(b)))
  }

  implicit def pathExtendRouteBuilder[L, A, AOut](implicit
    asA: AsPathParser.Aux[A, AOut],
    fuse: Fuse[L, AOut]
  ) = new RouteBuilderCombiner[RouteBuilder[L], A] {
    type Out = RouteBuilder[fuse.Out]
    def apply(builder: RouteBuilder[L], a: A): Out = RouteBuilder.cons(builder, CellParser(asA(a)))
  }

}

//mixed into package object
trait PathParsing extends LowPriorityPathParsing { 

  //lift strings and extractions to route builders so you can do "foo" to {...
  //this isn't in RouteBuilderOpsContainer so we can keep that as generic as possible
  implicit val stringAsBuilder = new AsRouteBuilder[String] {
    type Out = Unit
    def apply(s: String): RouteBuilder[Unit] = RouteBuilder.one(CellParser(new ConstantPrefixPath(s :: Nil)))
  }

  implicit val methodAsBuilder = new AsRouteBuilder[Method] {
    type Out = Unit
    def apply(m: Method): RouteBuilder[Unit] = RouteBuilder.one(CellParser(m))
  }
  implicit val exactMatchBuilder = new AsRouteBuilder[ExactMatchPath] {
    type Out = Unit
    def apply(e: ExactMatchPath): RouteBuilder[Unit] = RouteBuilder.one(CellParser(e))
  }

  /*
  implicit val methodCell = new AsCellComponent[RequestContext,Unit, Method] {
    def apply(m: Method): CellComponent[RequestContext,Unit] = CellParser(m)
  }
  */

  implicit val exactMatchCell = new AsCellComponent[RequestContext, Unit, ExactMatchPath] {
    def apply(p: ExactMatchPath) = CellParser(p)
  }

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
