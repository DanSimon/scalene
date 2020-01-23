package scalene.benchmark

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import scalene.actor.Pool
import scalene.routing._
import scalene.http.{BodyData, BodyFormatter, ContentType}
import scalene.sql._
import BasicConversions._

object Main extends App {

  trait JsonMessage
  case class JsonRouteMessage(message: String) extends JsonMessage
  case class DBRouteMessage(id: Int, randomnumber: Int) extends JsonMessage
  case class MultiDBRouteMessage(items: Array[DBRouteMessage]) extends JsonMessage

  implicit val messageFormatter = new BodyFormatter[JsonMessage] {
    val mapper: ObjectMapper = new ObjectMapper().registerModule(DefaultScalaModule)
    def format(msg: JsonMessage) = {
      val obj = msg match {
        case MultiDBRouteMessage(items) => items
        case other => other
      }
      BodyData.Static(mapper.writeValueAsBytes(obj))
    }
    val contentType = Some(ContentType.`application/json`)
  }

  val settings = Settings.basic(
    serverName = "scalene",
    port = 9876,
    server = ServerSettings.Default.copy(numWorkers = Some(8))
  )

  
  implicit val pool = new Pool
  val worldClient = MiniSQL.client("world", "jdbc:postgresql://localhost:5432/hello_world", "benchmarkdbuser", "benchmarkdbpass", None)

  val random = new java.util.Random
  
  def randomWorld(session: MiniSQLSession): Option[DBRouteMessage] = {
    val stmt = session.prepared("SELECT id, randomnumber FROM world WHERE id = (?)")
    stmt.setInt(1, math.abs(random.nextInt) % 10000)
    val rs = stmt.executeQuery()
    if (rs.next()) {
      Some(DBRouteMessage(rs.getInt(1), rs.getInt(2)))
    } else {
      None
    }      

  }

  val dbRoute = GET / "db" to {_ =>
    worldClient.query{session =>
      randomWorld(session).map{_.ok}.getOrElse("N/A".notFound)
    }
  }

  val QueryNum = optional(![String]).map{strOpt =>
    try {
      val i = strOpt.get.toInt
      if (i < 1) 1 else if (i > 500) 500 else i
    } catch {
      case e: Exception => 1
    }
  }

  val multiRoute = (GET / "queries" / QueryNum) to {num =>
    worldClient.query{session =>
      val worlds = new Array[DBRouteMessage](num)
      var i  = 0
      while (i < num) {
        worlds(i) = randomWorld(session).get
        i += 1
      }
      MultiDBRouteMessage(worlds).ok
    }
  }

  val routes = Routes(
    GET / "plaintext" as "Hello, World".ok,
    GET / "json"      as JsonRouteMessage("Hello, World").ok,
    dbRoute,
    multiRoute
  )

  Routing.start(settings, routes)

}
