package scalene.routing

import scalene.actor._
import scalene._
import scalene.http._
import scalene.corerouting._

object Routing {

  def startDetached(settings: HttpServerSettings, routes: HttpRoute)(implicit pool: Pool): Server = {
    //val built = routes.toRoute
    HttpServer.start(settings, implicit context => new RequestHandler[HttpRequest, HttpResponse] {
    
      def onInitialize(context: RequestHandlerContext){}

      def handleRequest(request: HttpRequest): Async[HttpResponse] = routes(new RequestContext(request)) match {
        case Right(f) => f.resolve(context)
        case Left(reason) => Async.successful(HttpResponse(reason.reason.code, http.Body.plain(reason.message)))
      }

      def handleError(request: Option[HttpRequest], error: Throwable) = HttpResponse(ResponseCode.Error, http.Body.plain(error.toString))

    })

  }

  def start(settings: HttpServerSettings, routes:  HttpRoute): Unit = {
    implicit val p = new Pool
    val server = startDetached(settings, routes)
    p.join
  }

}


