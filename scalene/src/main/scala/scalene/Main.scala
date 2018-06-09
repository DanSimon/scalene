package scalene

import java.nio.ByteBuffer
import java.util.{Arrays, LinkedList}
import microactor.Pool

object Main extends App {

  val settings = HttpServerSettings(
    serverName = "scalene",
    server = ServerSettings(
      port = 9876,
      addresses = Nil,
      maxConnections = 4096,
      tcpBacklogSize = None,
      numWorkers = Some(1)
    )
  )

  val body = Body.plain("Hello, World!")

  HttpServer.start(settings, List(
    Method.Get.url("/plaintext").to(body.ok)
  ))


}
