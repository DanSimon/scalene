package scalene
package stream

trait Signal {
  def onSignaled(f: => Unit): Unit
}

class LiveSignal extends Signal {
  private var event: Option[() => Unit] = None
  private var signaled = false
  def onSignaled(f: => Unit): Unit = if (signaled) {
    f
  } else {
    event = Some(() => f)
  }

  def signal(): Unit = {
    event.foreach(f => f())
    event = None
  }
}

sealed trait PushResult
object PushResult {
  case object Ok extends PushResult
  case class Wait(signal: Signal) extends PushResult
  case class Error(reason: Throwable) extends PushResult
}

trait Receiver[T] {
  def push(item: T): PushResult

}

trait BackPressureHandler[T] {
  def receiver: Receiver[T]
  def onPause(): Unit
  def onResume(): Unit
  def onError(reason: Throwable): Unit

  def push(item: T): Unit = receiver.push(item) match {
    case PushResult.Ok => ()
    case PushResult.Wait(signal) => {
      onPause()
      signal.onSignaled{
        onResume()
      }
    }
    case PushResult.Error(reason) => {
      onError(reason)
    }
  }

}

trait Sink[T] extends Receiver[T] {
  //close the stream, triggering the result
  def close(): Unit  
  def error(reason: Throwable): Unit
}

trait Collector[T, E] extends Sink[T]{
  def result: Async[E]
}

object Sink {

  def blackHole[T]: Sink[T] = new Sink[T] {
    def push(item: T) = PushResult.Ok
    def close() = {}
    def error(reason: Throwable) = {}
  }
}

//trait for an object that will produce items that should be pushed to a downstream Sink.
trait LiveSource[T] {
  protected var downstream: Option[Sink[T]] = None

  def setDownstream(sink: Sink[T]): Unit = {
    downstream = Some(sink)
  }
}

class LiveSink[T] extends Sink[T] with LiveSource[T]{

  def push(item: T) = downstream
    .map{_.push(item)}
    .getOrElse(PushResult.Error(new Exception("push to uninitialized stream")))

  def close() = downstream.foreach{_.close()}

  def error(reason: Throwable) = downstream.foreach{_.error(reason)}
}

class MappedSink[A,B](mapper: A => B, downstream: Sink[B]) extends Sink[A] {

  def push(item: A) = downstream.push(mapper(item))

  def close() = downstream.close()

  def error(reason: Throwable): Unit = downstream.error(reason)
}

class StreamBuilder[I, T](val initial: LiveSource[I], mapper: I => T) extends Stream[T] {

  def map[U](f: T => U) = new StreamBuilder[I,U](initial, i => f(mapper(i)))

  def complete[E](collector: Deferred[Collector[T, E]]): Deferred[E] = collector.flatMap {c =>
    initial.setDownstream(new MappedSink(mapper, c))
    ConstantDeferred(c.result)
  }

}

object StreamBuilder { 
  def apply[T](initial: LiveSource[T]): StreamBuilder[T, T] = new StreamBuilder(initial, x => x)
}


trait Stream[T] {
  def map[U](f: T => U): Stream[U]

  def complete[E](collector: Deferred[Collector[T, E]]): Deferred[E]

}

object Stream {

  def fromIter[T](iter: Iterator[T]): Stream[T] = {
    val source = new LiveSource[T] {
      override def setDownstream(sink: Sink[T]): Unit = {
        super.setDownstream(sink)
        continueDrain()
      }


      def continueDrain(): Unit = {
        var continue = true
        while (continue && iter.hasNext) {
          downstream.get.push(iter.next) match {
            case PushResult.Ok => {}
            case PushResult.Wait(signal) => {
              signal.onSignaled{
                continueDrain()
              }
              continue = false
            }
            case PushResult.Error(err) => {
              continue = false
            }
          }
        }
      }
    }
    StreamBuilder(source)
  }

}


