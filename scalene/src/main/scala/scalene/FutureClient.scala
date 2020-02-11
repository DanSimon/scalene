package scalene

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

import scalene.actor._
import util._

class FutureClient[Request, Response](
  factory: AsyncContext => BasicClient[Request, Response],
  config: BasicClientConfig
)(implicit pool: Pool) {

  implicit val dispatcher = pool.createDispatcher("future-client-{ID}")
  val timeKeeper = new RealTimeKeeper

  val receiver = SimpleReceiver[EventLoopEvent]{e => ()}

  val eventLoop = new EventLoop(EventLoopConfig.Default, timeKeeper, receiver)

  sealed trait ReceiverMessage
  case class AsyncRequest(request: Request, promise: Promise[Response]) extends ReceiverMessage
  case class ClientReady(client: BasicClient[Request, Response]) extends ReceiverMessage

  val sender = {
    var client: Option[BasicClient[Request, Response]] = None
    val buffer = new java.util.LinkedList[AsyncRequest]()
    SimpleReceiver[ReceiverMessage]{
      case ClientReady(c) => {
        client = Some(c)
        while (!buffer.isEmpty) {
          val n = buffer.remove()
          c.send(n.request).onComplete{t => n.promise.complete(t)}
        }
      }
      case a @ AsyncRequest(request, promise) => {
        client match {
          case Some(c) => c.send(request).onComplete{t => promise.complete(t)}
          case None => buffer.add(a)
        }
      }
    }
  }

  dispatcher.execute {
    val client = eventLoop.attachAndConnect[BasicClient[Request,Response]](config.address, factory)
    sender.send(ClientReady(client))
  }

  def send(request: Request): Future[Response] = {
    val p = Promise[Response]()
    sender.send(AsyncRequest(request, p))
    p.future
  }  

}
