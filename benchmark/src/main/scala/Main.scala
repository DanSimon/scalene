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
  val worldClient = MiniSQL.client("world", "jdbc:postgresql://localhost:5432/hello_world", "benchmarkdbuser", "benchmarkdbpass")

  val random = new java.util.Random
  val dbRoute = GET / "db" to {_ =>
    worldClient.query{session =>
      val stmt = session.prepared("SELECT id, randomnumber FROM world WHERE id = (?)")
      stmt.setInt(1, math.abs(random.nextInt) % 10000)
      val rs = stmt.executeQuery()
      if (rs.next()) {
        DBRouteMessage(rs.getInt(1), rs.getInt(2)).ok
      } else {
        "(N/A)".notFound
      }      
    }
  }


  val routes = Routes(
    GET / "plaintext" as "Hello, World".ok,
    GET / "json"      as JsonRouteMessage("Hello, World").ok,
    dbRoute    
  )

  Routing.start(settings, routes)

}
