package scalene

import microactor._
import util._

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

}
