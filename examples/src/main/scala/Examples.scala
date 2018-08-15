package examples

import scalene.routing._

object Main extends App {

  //val settings = Settings.basic(name = "examples", port = 8080)

  case class Foo(i: Int, s: String)
  val parseFooFromPath = ![Int] / ![String] >> Foo

  val fooRoutes = "foo" subroutes {base =>
    base + POST + parseFooFromPath to {foo => s"got a foo $foo".ok}
  }

  val routes = Routes(
    GET / "sum" / ![Int] / ![Int] to {case (a,b) => (a + b).ok},
    GET / "quotient" / ![Int] / ![Int].filter{_ != 0} to {case (a,b) => (a / b).ok}
  )

}
