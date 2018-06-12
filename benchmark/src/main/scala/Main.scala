package scalene.benchmark



import java.nio.ByteBuffer
import java.util.{Arrays, LinkedList}
import microactor.Pool
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
    server = ServerSettings(
      port = 9876,
      addresses = Nil,
      maxConnections = 4096,
      tcpBacklogSize = None,
      numWorkers = Some(1),
      maxIdleTime = 10.seconds
    )
  )

  HttpServer.start(settings, List(
    Get url "/plaintext"  to Body.plain("Hello, World!").ok,
    Get url "/json"       to JsonMessage("Hello, World!").ok
  ))


}
