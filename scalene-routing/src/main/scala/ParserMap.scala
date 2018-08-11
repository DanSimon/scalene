package scalene.routing

import scala.annotation.implicitNotFound

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._

@implicitNotFound(".map: ${F} either not a function or does not have arguments matching structure of ${A}")
trait ParserMap[I,A,B,F] {
  def map(parser: Parser[I, A], f: F) : Parser[I, B]
}

object ParserMap {

  implicit def mapOpen[I, A <: HList, B, F](implicit fp: FnToProduct.Aux[F, A => B]) = new ParserMap[I, A, B, F] {
    def map(parser: Parser[I, A], f: F): Parser[I, B] = new Parser[I, B] {
      val productized = f.toProduct
      def parse(input: I): Result[B] = parser.parse(input).map(productized)
    }
  }

  implicit def mapClosed[I, A, B, F <: A => B] = new ParserMap[I, A, B, F] {
    def map(parser: Parser[I, A], f: F): Parser[I, B] = new Parser[I, B] {
      def parse(input: I): Result[B] = parser.parse(input).map(f)
    }
  }

}
