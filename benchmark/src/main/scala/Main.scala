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
    def format(msg: JsonMessage) = BodyData.Static(mapper.writeValueAsBytes(msg))
    val contentType = Some(ContentType.`application/json`)
  }

  val settings = Settings.basic(
    serverName = "scalene",
    port = 9876,
    server = ServerSettings.Default//.copy(numWorkers = Some(1))
  )

  
  implicit val pool = new Pool
  val worldClient = MiniSQL.client2("world", "jdbc:postgresql://localhost:5432/hello_world", "benchmarkdbuser", "benchmarkdbpass", 12)

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

  val multiRoute = (GET / "queries" / ![Int])  to {num => //+ ?("queries",![Int]) to {num =>
    worldClient.query{session =>
      val worlds = new Array[DBRouteMessage](num)
      var i  = 0
      while (i < num) {
        worlds(i) = randomWorld(session).get//val worlds = (0 to 1).flatMap{i => randomWorld(session)}.toArray
        i += 1
      }
      MultiDBRouteMessage(worlds).ok
      //"ok".ok
    }
  }

  import C._

  val processor = new SimpleWorkerClient[String, String](4, () => _.toUpperCase)

  val processRoute = GET / "process" / ![String] to {str =>
    processor.send(str).map{_.ok}
    
  }



  val routes = Routes(
    GET / "plaintext" as "Hello, World".ok,
    GET / "json"      as JsonRouteMessage("Hello, World").ok,
    dbRoute,
    multiRoute,
    processRoute,
    GET / "shutdown" to {_ => processor.shutdown; "done".ok}
  )

  Routing.start(settings, routes)

}
