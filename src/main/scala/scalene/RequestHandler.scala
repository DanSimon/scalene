package scalene

trait RequestHandler[I,O] {

  def handleRequest(request: I): Async[O]

  def handleError(request: Option[I], error: Throwable): O

}


