package examples

import scalene.routing._
import BasicConversions._

object Main extends App {

  case class Foo(i: Int, s: String)
  val parseFooFromPath = ![Int] / ![String] map Foo.tupled

  val fooRoutes = "foo" subroutes (
    _ + POST + parseFooFromPath to {foo => s"got a foo $foo".ok},
    _ + GET + ![Int].filter{_ > 0} to {id => s"give me foo $id".ok}
  )

  val routes = Routes(
    fooRoutes,
    GET / "sum" / ![Int] / ![Int] to {case (a,b) => (a + b).ok},
    GET / "quotient" / ![Int] / ![Int].filter{_ != 0} to {case (a,b) => (a / b).ok}
  )
  
  val settings = Settings.basic(serverName = "examples", port = 8080)
  Routing.start(settings, routes)

}
