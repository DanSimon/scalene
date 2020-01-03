package scalene

import scalene.actor._
import util._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

trait AsyncContext {
  def localized[T](key: UniqueKey[T])(createor: => T): T
  def eventLoop: EventLoop
  def time: TimeKeeper
  def timer: Timer

  def futureToAsync[T](f: Future[T]): Async[T]
  def threadSafePromise[T](): (Try[T] => Unit, Async[T])
}

class AsyncContextImpl (
  val time: TimeKeeper,
  val timer: Timer,
  _dispatcher: Dispatcher,
  val eventLoop: EventLoop
) extends AsyncContext {

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
      futureToAsyncExecutor.send(() => {p.complete(t)})
    }
    p
  }

  def threadSafePromise[T](): (Try[T] => Unit, Async[T]) = {
    val promise = new PromiseAsync[T]
    val func = (t: Try[T]) => {
      futureToAsyncExecutor.send(() => promise.complete(t))
      ()
    }
    (func, promise)
  }

    

}
