package scalene

import microactor._
import util._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class AsyncContext(
  val time: TimeKeeper,
  val timer: Timer,
  _dispatcher: Dispatcher,
  val eventLoop: EventLoop
) {

  private val cache = collection.mutable.Map[GenericUniqueKey, Any]()

  implicit def dispatcher = _dispatcher

  def localized[T](key: UniqueKey[T])(creator: => T): T = if (cache.contains(key)) {
    cache(key).asInstanceOf[T]
  } else {
    val local = creator
    cache(key) = local
    local
  }

  val futureToAsyncExecutor = SimpleReceiver[() => Unit]{f => f()}

  def futureToAsync[T](f: Future[T]): Async[T] = {
    val p = new PromiseAsync[T]
    f.onComplete{t => 
      futureToAsyncExecutor.send(() => p.complete(t))
    }
    p
  }

}
