package examples

import scalene.routing._

class MyController {

  def sum(a: Int, b: Int): Int = a + b

}

object Main extends App {

  val settings = HttpServerSettings.basic(name = "examples", port = 8080)

  val controller = new MyController

  val routes = Routes(
    GET + Path(RT / "sum" / ![Int] / ![Int]) to controller.sum,
    PUT + Path(R
