package scalene.benchmark

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import scala.concurrent.duration._
import scalene._
import scalene.http._
import scalene.routing._
import scalene.httprouting._
import Body.BodyLifting

object Main extends App {

  case class JsonMessage(message: String)

  implicit val messageFormatter = new BodyFormatter[JsonMessage] {
    val mapper: ObjectMapper = new ObjectMapper().registerModule(DefaultScalaModule)
    def format(msg: JsonMessage) = mapper.writeValueAsBytes(msg)
    val contentType = Some(ContentType.`application/json`)
  }

  val settings = HttpServerSettings(
    serverName = "scalene",
    maxIdleTime = 60.seconds,
    server = ServerSettings(
      port = 9876,
      addresses = Nil,
      maxConnections = 4096,
      tcpBacklogSize = None,
      numWorkers = Some(1),
    )
  )

  val plainBody = Body.plain("Hello, World")

  val routes = Routes(
    GET / "plaintext" as plainBody.ok,
    GET / "json"      as JsonMessage("Hello, World").ok
  )

  val route = Routes(
    GET / "plaintext" as plainBody.ok
  )

  //val route = Parameter("foo", ![Int]) + Parameter("bar", ![Int]) to {case (a,b) => (a + b).toString.ok}

  Routing.start(settings, routes)

}
