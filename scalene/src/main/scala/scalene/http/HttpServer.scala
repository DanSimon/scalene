package scalene
package http

import java.util.{Arrays, LinkedList}
import scala.concurrent.duration._
import microactor._

import util._

class BasicRoute(val method: Method, val fullUrl: String, val handler: HttpRequest => Deferred[HttpResponse]) {
  private val flMatch = s"${method.name.toUpperCase} $fullUrl HTTP/1.1".getBytes

  def isMatch(req: HttpRequest) = Arrays.equals(req.firstLine, flMatch)

}

class BasicRouter(routeSeq: Seq[BasicRoute], ctx: AsyncContext) extends RequestHandler[HttpRequest, HttpResponse] {
  var _context: Option[RequestHandlerContext] = None

  private val routes = routeSeq.toArray
  private val NoRouteResponse = Async.successful(HttpResponse(ResponseCode.NotFound, Body.plain("unknown path")))

  def handleRequest(input: HttpRequest) = {
    var i = 0
    while (i < routes.length && !routes(i).isMatch(input)) { i += 1 }
    if (i < routes.length) {
      routes(i).handler(input).resolve(ctx)
    } else {
      NoRouteResponse
    }
  }

  def handleError(req: Option[HttpRequest], reason: Throwable) = HttpResponse(
    ResponseCode.Error,
    Body.plain(reason.getMessage)
  )

  override def onInitialize(ctx: RequestHandlerContext): Unit = {
    _context = Some(ctx)
  }

}

case class HttpServerSettings(
  maxIdleTime: Duration,
  serverName: String,
  server: ServerSettings,
  commonHeaders: Seq[Header] = List(new DateHeader)
)

object HttpServer {
  def start(settings: HttpServerSettings, routes: Seq[BasicRoute])(implicit pool:Pool = new Pool): Server = {
    //implicit val pool = new Pool
    val commonHeaders = (settings.commonHeaders :+ Header("Server", settings.serverName)).toArray
    val factory: AsyncContext => ServerConnectionHandler = ctx => {
      new ServiceServer((x: HttpRequest => Unit) => 
          new HttpServerCodec(x, ctx.time, commonHeaders), new BasicRouter(routes, ctx), settings.maxIdleTime)
    }
    Server.start(settings.server, factory, new RefreshOnDemandTimeKeeper(new RealTimeKeeper))
  }
}

case class RouteBuilding2(method: Method, url: String) {
  def to(handler: HttpRequest => Deferred[HttpResponse]) : BasicRoute = {
    new BasicRoute(method, url, handler)
  }

  def to(syncResponse: => HttpResponse): BasicRoute = to(_ => ConstantDeferred(Async.successful(syncResponse)))

  def to(deferredResponse: Deferred[HttpResponse]): BasicRoute = to(_ => deferredResponse)

  def respondWith(response: HttpResponse): BasicRoute = to(response)
}
