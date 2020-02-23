package scalene
package http

import java.util.{Arrays, LinkedList}
import scala.concurrent.duration._
import scalene.actor._

import util._


case class HttpServerSettings(
  maxIdleTime: Duration,
  serverName: String,
  server: ServerSettings,
  commonHeaders: Seq[Header] = List(new DateHeader)
)

object HttpServerSettings {

  def basic(
    serverName: String = "scalene",
    port: Int = 8080,
    commonHeaders: Seq[Header] = List(new DateHeader),
    server: ServerSettings = ServerSettings.Default,
    maxIdleTime: Duration = 10.seconds
  ) = HttpServerSettings(maxIdleTime, serverName, server.copy(port = port), commonHeaders)

}

object HttpServer {

  type HttpRequestHandler = RequestHandler[HttpRequest, HttpResponse]

  def start(settings: HttpServerSettings, requestHandlerFactory: AsyncContext => HttpRequestHandler)(implicit pool: Pool): Server = {
    val commonHeaders = (settings.commonHeaders :+ Header("Server", settings.serverName)).toArray
    val factory: AsyncContext => ServerConnectionHandler = ctx => {
      new ServiceServer(ctx, (x: HttpRequest => Unit) => 
          new HttpServerCodec(x, ctx.time, commonHeaders), requestHandlerFactory(ctx), settings.maxIdleTime)
    }
    Server.start(settings.server, factory, new RefreshOnDemandTimeKeeper(new RealTimeKeeper))
  }
}

