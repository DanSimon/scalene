package scalene.benchmark

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import scalene.routing._
import scalene.http.{BodyFormatter, ContentType}
import BasicConversions._

object Main extends App {

  case class JsonMessage(message: String)

  implicit val messageFormatter = new BodyFormatter[JsonMessage] {
    val mapper: ObjectMapper = new ObjectMapper().registerModule(DefaultScalaModule)
    def format(msg: JsonMessage) = mapper.writeValueAsBytes(msg)
    val contentType = Some(ContentType.`application/json`)
  }

  val settings = Settings.basic(
    serverName = "scalene",
    port = 9876,
    server = ServerSettings.Default.copy(numWorkers = Some(1))
  )

  val routes = Routes(
    GET / "plaintext" as "Hello, World".ok,
    GET / "json"      as JsonMessage("Hello, World").ok
  )

  Routing.start(settings, routes)

}
