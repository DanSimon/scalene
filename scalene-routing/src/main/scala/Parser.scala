package scalene.routing

import scalene.Deferred
import scalene.http._
import scala.annotation.implicitNotFound

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._

trait Parser[I, O] {
  def parse(input: I) : Result[O]

  def >>[B, F](f: F)(implicit mapper : ParserMap[I, O, B, F]) : Parser[I, B] = mapper.map(this, f)

  def map[B](f: O => B): Parser[I,B] = ParserMap.mapClosed[I,O,B,O => B].map(this, f)

  def flatMap[B](f: O => Parser[I,B]) = {
    val me = this
    new Parser[I,B] {
      def parse(input: I): Result[B] = for {
        myres       <- me.parse(input)
        flatmapped  = f(myres)
        mappedres   <- flatmapped.parse(input)
      } yield mappedres
    }
  }


  def mapParser[B](p: Parser[O,B]): Parser[I,B] = {
    val me = this
    new Parser[I,B] {
      def parse(input: I): Result[B] = me.parse(input).flatMap(p.parse)
    }
  }

  def filter(failMessage: O => String,f: O => Boolean): Parser[I,O] = {
    val me = this
    new Parser[I,O] {
      def parse(input: I): Result[O] = me
        .parse(input)
        .flatMap{res => if (f(res)) Right(res) else Left(ParseError.badRequest(failMessage(res)))}
    }
  }
  
  def filter(f: O => Boolean): Parser[I,O] = filter((x: O) => s"parse result '$x' failed filter", f)

}

trait Formatter[T] extends Parser[String,T]{
  def format(data: String): Result[T]

  def parse(input: String): Result[T] = format(input)
}

object StringF extends Formatter[String] {
  def format(data: String) = Right(data)
}
object IntF extends Formatter[Int] {
  def format(data: String) = try { Right(data.toInt) } catch {
    case e : Throwable => Left(ParseError(ErrorReason.BadRequest, () => e.getMessage))
  }
}
object BooleanF extends Formatter[Boolean] {
  def format(data: String) = 
    if (data.toLowerCase == "true") Right(true) 
    else if (data.toLowerCase == "false") Right(false)
    else Left(ParseError(ErrorReason.BadRequest, () => s"expected true/false, got '${data}'"))
}


//just a parser that returns the whole request
object FullRequest extends Parser[RequestContext, HttpRequest] {
  def parse(input: RequestContext) = Right(input.request)
}

trait Clonable[T] {
  def cclone : T
}

trait Filter[I,O] {
  def apply(input: I): Deferred[O]
}
object Filter {
  def apply[I,O](f: I => Deferred[O]) = new Filter[I,O] {
    def apply(input: I) = f(input)
  }
}
