package scalene
package http

import java.util.{Arrays, LinkedList}
import microactor._

import util._

class BasicRoute(val method: Method, val fullUrl: String, val handler: HttpRequest => Async[HttpResponse]) {
  private val flMatch = s"${method.name.toUpperCase} $fullUrl HTTP/1.1".getBytes

  def isMatch(req: HttpRequest) = Arrays.equals(req.firstLine, flMatch)

}

class BasicRouter(routeSeq: Seq[BasicRoute]) extends RequestHandler[HttpRequest, HttpResponse] {
  var _context: Option[RequestHandlerContext] = None

  private val routes = routeSeq.toArray
  private val NoRouteResponse = Async.successful(HttpResponse(ResponseCode.NotFound, Body.plain("unknown path")))

  def handleRequest(input: HttpRequest) = {
    var i = 0
    while (i < routes.length && !routes(i).isMatch(input)) { i += 1 }
    if (i < routes.length) {
      routes(i).handler(input)
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
  serverName: String,
  server: ServerSettings,
  commonHeaders: Seq[Header] = List(new DateHeader)
)

object HttpServer {
  def start(settings: HttpServerSettings, routes: Seq[BasicRoute])(implicit pool:Pool = new Pool): Server = {
    //implicit val pool = new Pool
    val commonHeaders = (settings.commonHeaders :+ Header("Server", settings.serverName)).toArray
    val factory: WorkEnv => ServerConnectionHandler = ctx => {
      new ServiceServer((x: HttpRequest => Unit) => 
          new HttpServerCodec(x, ctx.time, commonHeaders), new BasicRouter(routes))
    }
    Server.start(settings.server, factory, new RefreshOnDemandTimeKeeper(new RealTimeKeeper))
  }
}

case class RouteBuilding2(method: Method, url: String) {
  def to(handler: HttpRequest => Async[HttpResponse]) : BasicRoute = {
    new BasicRoute(method, url, handler)
  }

  def to(syncResponse: => HttpResponse): BasicRoute = to(_ => Async.successful(syncResponse))

  def respondWith(response: HttpResponse): BasicRoute = to(response)
}
