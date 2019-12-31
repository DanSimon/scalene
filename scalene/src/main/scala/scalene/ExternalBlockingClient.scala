package scalene

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

import scalene.actor._
import util._

class ExternalBlockingClient[Request, Response](
  client: Request => Try[Response]
)(implicit pool: Pool) {

  implicit val dispatcher = pool.createDispatcher
  val timeKeeper = new RealTimeKeeper


  case class AsyncRequest(request: Request, promise: Promise[Response])

  val receiver = SimpleReceiver[AsyncRequest]{request =>
    request.promise.complete(client(request.request))
  }

  def send(request: Request): Deferred[Response] = defer{ implicit context =>
    val p = Promise[Response]()
    receiver.send(AsyncRequest(request, p))
    context.futureToAsync(p.future)
  }  

}

