package scalene.routing

import scalene._
import http.{Method => HttpMethod, _}
import scala.annotation.implicitNotFound
import java.util.Arrays

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._

@implicitNotFound("implicit ParserFolder[$H, $I] not found, probably means you're trying to add something that isn't a Parser[$I,_]")
trait ParserFolder[H <: HList, I] {
  type Out <: HList
  def process(input: I, list: H): Result[Out]
}

object ParserFolder {

  type Aux[H <: HList,I,O <: HList] = ParserFolder[H,I] { type Out = O }

  implicit def foldHNil[I] = new ParserFolder[HNil, I] {
    type Out = HNil
    def process(input: I, list: HNil) = {
      Right(HNil)
    }
  }

  implicit def foldCons[L <: HList, I, O, OUT <: HList](implicit 
    tailFold: ParserFolder.Aux[L, I, OUT],  //get the folder for the tail
    fuser: Fuse[O, OUT]           //fuser to prepend O to Out
  ) = new ParserFolder[Parser[I,O] :|: L, I] {
    type Out = fuser.Out
    def process(input: I, list: Parser[I,O] :|: L) = for {
      res           <- list.head.parse(input)
      tailresult    <- tailFold.process(input, list.tail)
    } yield fuser.fuse(res, tailresult)
  }

}

case class PathList[H <: HList , Out <: HList]
(list: H)
(implicit val folder : ParserFolder.Aux[H, PathIn, Out])
extends Parser[PathIn, Out] {

  def /[E, T, O <: HList, NO <: HList](extractor: E)(implicit
    provider: PathExtractorProvider.Aux[E, T],
    fuser: Fuse.Aux[H, PathParser[T], O],
    folder: ParserFolder.Aux[O, PathIn, NO]
  ) = PathList[O, NO](fuser.fuse(list, (provider.provide(extractor) : PathParser[T])))

  def parse(input: PathIn): Result[Out] = folder.process(input, list)
}

object RT extends PathList[HNil, HNil](HNil)

object PathExtractor {

  type PathExtractor[T] = Parser[String, T]

  def extract[T](implicit formatter: Formatter[T]): PathExtractor[T] = formatter


  def matchLiteral[T](literal : T)(implicit formatter: Formatter[T]): PathExtractor[HNil] = 
    formatter.filter(x => s"expected $literal, got $x", _ == literal).map{_ => HNil}
}

@implicitNotFound("Need an implicit Parser[String ${T}] in scope")
trait PathExtractorProvider[T] {
  type Out
  def provide(extraction: T): PathParser[Out]
}

object PathExtractorProvider {

  type Aux[T,O] = PathExtractorProvider[T] { type Out = O }

  implicit def literal[T : Formatter] : PathExtractorProvider.Aux[T, HNil] = new PathExtractorProvider[T] {
    type Out = HNil
    def provide(literal: T) = SinglePathParser(PathExtractor.matchLiteral(literal))
  }

  implicit def extract[T : Formatter] : PathExtractorProvider.Aux[Extract[T], T] = new PathExtractorProvider[Extract[T]] {
    type Out = T
    def provide(e : Extract[T]) = SinglePathParser(implicitly[Formatter[T]])
  }

  implicit def selfProvider[T] = new PathExtractorProvider[PathParser[T]] {
    type Out = T
    def provide(heylookitsme: PathParser[T]) = heylookitsme
  }
}

case class SinglePathParser[T](formatter: Parser[Raw,T]) extends PathParser[T] {
  def parse(components: PathIn): Result[T] = if (components.hasNext) {
    formatter.parse(components.next).left.map{e => e.copy(reason = ErrorReason.NotFound)}
  } else {
    Left(ParseError.notFound("expected component"))
  }
}

class ExactMatchPath(method: HttpMethod, fullPath: String) extends Parser[RequestContext, HNil] {
  val bytes = s"${method.name.toUpperCase} $fullPath".getBytes
  val ok = Right(HNil)
  def parse(req: RequestContext): Result[HNil] = {
    if (Arrays.equals(req.request.firstLine, 0, bytes.length, bytes, 0, bytes.length)) {
      ok
    } else {
      Left(ParseError.notFound("Not a match"))
    }
  }
}

//this just exists to make it easier to define path parsers in-line
object Path {
  def apply[H <: HList](parser: Parser[PathIn, H]): Parser[RequestContext, H] = new Parser[RequestContext, H] {
    def parse(req: RequestContext): Result[H] = {
      parser.parse(req)
    }
  }

  def exact(method: HttpMethod, fullPath: String) = new ExactMatchPath(method, fullPath)
}

case class Url(url: String) extends Parser[RequestContext, HNil] {
  def parse(req: RequestContext): Result[HNil] = if (req.request.url == url) Right(HNil) else {
    Left(ParseError.notFound("Not a match"))
  }
}


object NewPath {

  trait PathParser[T] extends Parser[RequestContext, T]

  class ConstantPrefixPath(prefixPieces: List[String]) extends PathParser[HNil] {
    val pieces = prefixPieces.flatMap{_.split("/")}.filter{_ != ""}
    val prefix = "/" + pieces.mkString("/")

    def parse(ctx: RequestContext): Result[HNil] = ???

  }

  class ExtractionSegmentParser[T](formatter: Parser[Raw,T]) extends PathParser[T] {
    def parse(components: RequestContext): Result[T] = if (components.hasNext) {
      formatter.parse(components.next).left.map{e => e.copy(reason = ErrorReason.NotFound)}
    } else {
      Left(ParseError.notFound("expected component"))
    }
  }

  implicit class PathCombine[A](val a: A) {
    def /[B](b: B)(implicit com: RouteBuilderCombiner[A,B]): com.Out = com(a,b)
  }

  object PathCombine {
    //TODO : Strings might be "a/b" form
    //


    implicit def combineStringString = new RouteBuilderCombiner[String, String] {
      type Out = ConstantPrefixPath
      def apply(a: String, b: String): ConstantPrefixPath = new ConstantPrefixPath(a :: b :: Nil)
    }

    implicit def combinePrefixString = new RouteBuilderCombiner[ConstantPrefixPath, String] {
      type Out = ConstantPrefixPath
      def apply(a: ConstantPrefixPath, b: String): ConstantPrefixPath = new ConstantPrefixPath(a.pieces :+ b)
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
      def apply(a: String, b: Extract[A]): Out = comb(new ConstantPrefixPath(a :: Nil), new ExtractionSegmentParser(formatter))
    }

  }

  import PathCombine._

  val test1 = "foo" / "bar"

  val test2 = "foo" / ![Int] / "baz"





}

