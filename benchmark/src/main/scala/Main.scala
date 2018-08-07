package scalene.benchmark



import java.nio.ByteBuffer
import java.util.{Arrays, LinkedList}
import scalene.actor.Pool
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import scala.concurrent.duration._
import scalene._
import scalene.http._
import Body.BodyLifting
import Method._

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

  val plainBody = Body("Hello, World!".getBytes, Some(ContentType.`text/plain`))

  HttpServer.start(settings, List(
    Get url "/plaintext"  to plainBody.ok,
    Get url "/json"       to JsonMessage("Hello, World").ok
  ))

  val client = HttpClient.deferredClient(BasicClientConfig.default("localhost", 9876))

  HttpServer.start(settings.copy(serverName = "proxy", maxIdleTime = Duration.Inf, server = settings.server.copy(port = 6789)), List(
    Get url "/plaintext" to client.send(HttpRequest.get("/plaintext"))
  ))


}
