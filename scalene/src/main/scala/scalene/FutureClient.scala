package scalene

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

import microactor._
import util._

class FutureClient[Request, Response](
  factory: AsyncContext => BasicClient[Request, Response],
  config: BasicClientConfig
)(implicit pool: Pool) {

  implicit val dispatcher = pool.createDispatcher
  val timeKeeper = new RealTimeKeeper

  val receiver = SimpleReceiver[EventLoopEvent]{e => ()}

  val eventLoop = new EventLoop(timeKeeper, receiver)

  val client = eventLoop.attachAndConnect[BasicClient[Request,Response]](config.address, factory)

  case class AsyncRequest(request: Request, promise: Promise[Response])

  val sender = SimpleReceiver[AsyncRequest]{req =>
    client.send(req.request).onComplete{t => req.promise.complete(t)}
  }

  def send(request: Request): Future[Response] = {
    val p = Promise[Response]()
    sender.send(AsyncRequest(request, p))
    p.future
  }  

}
