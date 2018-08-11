package scalene

import scalene.http.{Method => HttpMethod, _}
import scala.annotation.implicitNotFound
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Try, Success, Failure}

import shapeless.{:: => :|:, _}
import ops.hlist._
import syntax.std.function._
import ops.function._


package object routing extends RouteBuilderOps[HttpResponse] {

  type Raw = String //convert to databuffer eventually

  type Result[T] = Either[ParseError, T]

  type ContinuedParser[I,O] = Parser[I, (I,O)]
  type PathIn = Iterator[String]
  type PathParser[O] = Parser[PathIn, O]
  type RequestParser[O] = Parser[RequestContext, O]

  type RouteResult[O] = Result[Deferred[O]]
  type BuiltRoute[I,O] = I => RouteResult[O]

  type HttpRoute = Route[RequestContext, HttpResponse]

  object RequestFilter {
    def apply[O](f: RequestContext => Deferred[O]): Filter[RequestContext,O] = Filter(f)
  }

  object RequestParser {
    def apply[O](p: HttpRequest => Result[O]) = new Parser[RequestContext, O] {
      def parse(r: RequestContext) = p(r.request)
    }

  }

  //default formatters

  implicit val stringFormatter: Formatter[String] = StringF
  implicit val intF : Formatter[Int] = IntF
  implicit val boolF : Formatter[Boolean] = BooleanF

  //parser tokens
  

  /**
   * A "token" type used in parsers, indicates a value should be extracted at
   * this location
   */
  case class Extract[T]() 
  def ![T] = Extract[T]()


  /**
   * wildcard Accept-all path parser, should be used at the end of a path.
   */
  val * : PathParser[HNil] = new PathParser[HNil] {
    def parse(components: PathIn) = Right(HNil)
  }

  /**
   * Similar to the `*` parser but also captures the remainder as a string
   */
  val !* : PathParser[String] = new PathParser[String] {
    def parse(components: PathIn) = Right(components.mkString("/"))
  }

  val GET = Method(HttpMethod.Get)
  val POST = Method(HttpMethod.Post)


  implicit class Futurize[T <: Any](val thing: T) extends AnyVal {
    def future = Deferred.successful(thing)
  }

}
