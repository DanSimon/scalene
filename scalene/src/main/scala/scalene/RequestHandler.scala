package scalene

import util._

trait RequestHandlerContext {
  def time: TimeKeeper

  def closeConnection(): Unit

}

trait RequestHandler[I,O] {

  def onInitialize(context: RequestHandlerContext)

  def handleRequest(request: I): Async[O]

  def handleError(request: Option[I], error: Throwable): O

}


