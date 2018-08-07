package scalene.actor

import scala.concurrent.{Future, Promise}

final class Wrap[T](obj: T)(implicit dispatcher: Dispatcher) {

  private val runner = SimpleReceiver[() => Unit](f => f())

  final def apply(f: T => Unit) = runner.send(() => f(obj))
  final def get[U](f: T => U): Future[U] = {
    val p = Promise[U]()
    runner.send(() => p.success(f(obj)))
    p.future
  }

}

object Wrap {
  def apply[T](obj: T)(implicit d: Dispatcher): Wrap[T] = new Wrap(obj)
}
