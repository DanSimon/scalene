package examples

import scalene.Deferred
import scalene.http._
import scalene.routing._
import scalene.httprouting._

object Main extends App {

  //val settings = Settings.basic(name = "examples", port = 8080)
  implicit val intResponse = new AsResponse[Int, HttpResponse] {
    def apply(i: Int) = Deferred.successful(Body.plain(i.toString).ok)
  }
  implicit val stringResponse = new AsResponse[String, HttpResponse] {
    def apply(s: String) = Deferred.successful(Body.plain(s).ok)
  }

  case class Foo(i: Int, s: String)
  val parseFooFromPath = ![Int] / ![String] map Foo.tupled

  val fooRoutes = "foo" subroutes {base =>
    base + POST + parseFooFromPath to {foo => s"got a foo $foo"}
  }

  val routes = Routes(
    GET / "sum" / ![Int] / ![Int] to {case (a,b) => (a + b)},
    GET / "quotient" / ![Int] / ![Int].filter{_ != 0} to {case (a,b) => (a / b)}
  )

}
