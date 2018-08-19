package scalene.routing

import scalene.actor._
import scalene._
import scalene.http._
import scalene.httprouting._

object Routing {

  def start(settings: HttpServerSettings, routes: HttpRoute) = {
    implicit val p = new Pool
    //val built = routes.toRoute
    HttpServer.start(settings, implicit context => new RequestHandler[HttpRequest, HttpResponse] {
    
      def onInitialize(context: RequestHandlerContext){}

      def handleRequest(request: HttpRequest): Async[HttpResponse] = routes(new RequestContext(request)) match {
        case Right(f) => f.resolve(context)
        case Left(reason) => Async.successful(HttpResponse(reason.reason.code, Body.plain(reason.message)))
      }

      def handleError(request: Option[HttpRequest], error: Throwable) = HttpResponse(ResponseCode.Error, Body.plain(error.toString))

    })
    p.join

  }

}


