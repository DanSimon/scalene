package examples

import scalene.routing._
import scalene.stream._
import scalene.sql._
import scalikejdbc._
import BasicConversions._

object Main extends App {
  
  GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
    enabled = false,
  )

  case class Foo(i: Int, s: String)
  val parseFooFromPath = ![Int] / ![String] map Foo.tupled

  val PositiveInt = ![Int]("num")
    .filter(_ > 0, "{NAME} must be a positive integer")


  val streamRoute = GET / "stream" to {_ =>
    Stream.fromIter(List("a", "b" , "c").toIterator).ok
  }

  implicit val pool = new scalene.actor.Pool

  val worldClient = MiniSQL.client("world", "jdbc:postgresql://localhost:5432/hello_world", "benchmarkdbuser", "benchmarkdbpass")

  val random = new java.util.Random

  /*
  val dbroute = GET / "db" to {_ =>
    val id = random.nextInt() % 10000
  */

  val fooRoutes = GET / "foo" / ![Int] to {id =>
    worldClient.query{session =>
      val prep = session.prepared("SELECT randomnumber FROM world WHERE id = ?")
      prep.setInt(1, id)
      val rs = prep.executeQuery()
      if (rs.next()) {
        rs.getInt(1).toString
      } else {
        "(N/A)"
      }
    }.map{_.ok}
  }

  class MyHandler {
    var num = 0

    def incrementBy(i: Int): Int = {
      num += i
      num
    }

  }
  implicit val MyHandlerProvider = new AttachmentProvider[MyHandler] {
    def provide(ctx: scalene.RequestHandlerContext) = new MyHandler
  }
  

  val routes = Routes(
    fooRoutes,
    streamRoute,
    GET / "sum" / ![Int] / ![Int] to {case (a,b) => (a + b).ok},

    GET / "quotient" / ![Int] / 
      ![Int].filter(_ != 0, "dividend can't be zero") to {case (a,b) => (a / b).ok},

    GET / "shutdown" + Context to {ctx => 
      ctx.closeConnection
      "bye".ok
    },

    (GET / "increment" / optional(![Int])) + Attachment[MyHandler] to {
      case (Some(i), h) => h.incrementBy(i).ok
      case (None, h) => h.incrementBy(1).ok
    }

  )
  
  val settings = Settings.basic(serverName = "examples", port = 8080)
  Routing.start(settings, routes)

}
