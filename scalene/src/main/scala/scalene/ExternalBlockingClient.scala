package scalene

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

import scalene.actor._
import util._

abstract class ExternalBlockingClient[Request, Response](implicit pool: Pool) {

  implicit val dispatcher = pool.createDispatcher("EBC-{ID}")
  val timeKeeper = new RealTimeKeeper

  def sendBlocking(request: Request): Try[Response]


  case class AsyncRequest(request: Request, promise: Try[Response] => Unit)

  val receiver = SimpleReceiver[AsyncRequest]{request =>
    request.promise(sendBlocking(request.request))
  }

  def send(request: Request): Deferred[Response] = defer{ implicit context =>
    val (tPromise, async) = context.threadSafePromise[Response]()
    receiver.send(AsyncRequest(request, tPromise))
    async
  }  

}

