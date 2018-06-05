package scalene

import java.nio.ByteBuffer
import java.util.{Arrays, LinkedList}
import microactor.Pool

object Main extends App {

  val settings = ServerSettings(
    port = 9876,
    addresses = Nil,
    maxConnections = 500,
    tcpBacklogSize = None,
    numWorkers = Some(1)
  )

  val headers = Array(
    new DateHeader,
    Header("Server", "benchmark"), 
    Header("Content-Type", "text/plain")
  )
  val body = "Hello, World!".getBytes

  HttpServer.start(settings, List(
    new BasicRoute(Method.Get, "/plaintext", _ => Async.successful(BasicHttpResponse(ResponseCode.Ok, headers, body)))
  ))


}
