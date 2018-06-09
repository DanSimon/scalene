package scalene

import util._

case class RequestHandlerContext(time: TimeKeeper)

trait RequestHandler[I,O] {

  def onInitialize(context: RequestHandlerContext)

  def handleRequest(request: I): Async[O]

  def handleError(request: Option[I], error: Throwable): O

}


