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
    signaled = true
    event.foreach(f => f())
    event = None
  }

  def reset(): Unit = {
    signaled = false
    event = None
  }
}

sealed trait PushResult
object PushResult {
  case object Ok extends PushResult

  //the item was pushed, but backpressure has been signaled
  case class WaitAccepted(signal: Signal) extends PushResult

  //the item was not pushed due to backpressure.
  case class WaitRejected(signal: Signal) extends PushResult

  case class Error(reason: Throwable) extends PushResult
}

trait Receiver[T] {
  def push(item: T): PushResult
  def pushPeek(): PushResult

}

/*
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

  def pushPeek

}
*/


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

trait BufferedSink[T] extends Sink[T] {

  private val buffer = new java.util.LinkedList[T]
  private val externalSignal = new LiveSignal

  def maxBufferSize: Int = 1000

  override def push(item: T) = if (buffer.size > 0) {
    buffer.add(item)
  } else {
    super.push(item) match {
      case PushResult.Ok => PushResult.Ok
      case PushResult.WaitAccepted(_) => {
        PushResult.WaitAccepted(externalSignal)
      }
      case PushResult.WaitRejected(signal) => {
        buffer.add(item)
        signal.onSignal{
          drainBuffer()
        }
        PushResult.Ok
      }
      case PushResult.Error(err) => PushResult.Error(err)
    }
  }

  private def drainBuffer() {
    var continue = true
    while (continue && buffer.size > 0) {
      super.push(item) match {
        case PushResult.Ok => {}
        case PushResult.WaitAccepted(signal) => {
          signal.onSignal {
            drainBuffer()
          }
          continue = false
        }
        case PushResult.WaitRejected(_) => {
          //this should not happen
          throw new Exception("rejected push from buffer drain")
        }
        case PushResult.Error(err) => {
          continue = false
        }
      }
    }
  }       


  override def close() = super.close()
  override def error(reason: Throwable) = super.error(reason)
}



//class GluedSink[T](from: Sink[T], to: Sink[T]) extends Sink[T]

//trait for an object that will produce items that should be pushed to a downstream Sink.
trait LiveSource[T] {
  protected var downstream: Option[Sink[T]] = None
  protected val setSignal = new LiveSignal

  def setDownstream(sink: Sink[T]): Unit = {
    downstream = Some(sink)
    setSignal.signal()
  }
}

class LiveSink[T] extends Sink[T] with LiveSource[T]{

  def push(item: T) = downstream
    .map{_.push(item)}
    .getOrElse(PushResult.Wait(setSignal))

  def close() = downstream.foreach{_.close()}

  def error(reason: Throwable) = downstream.foreach{_.error(reason)}
}

class MappedSink[A,B](mapper: A => B, downstream: Sink[B]) extends Sink[A] {

  def push(item: A) = downstream.push(mapper(item))

  def close() = downstream.close()

  def error(reason: Throwable): Unit = downstream.error(reason)
}

case class InitialStreamBuilder[I, T](initial: LiveSource[I], mapper: I => T) extends Stream[T] {

  def map[U](f: T => U) = InitialStreamBuilder[I,U](initial, i => f(mapper(i)))

  def complete(sink: Sink[T]): Unit = {
    initial.setDownstream(new MappedSink(mapper, sink))
  }

}
object InitialStreamBuilder {
  def apply[T](initial: LiveSource[T]): InitialStreamBuilder[T, T] = InitialStreamBuilder(initial, x => x)
}

case class StreamBuilder[I, T](initial: LiveSink[I], mapper: I => T) extends Stream[T] {

  def map[U](f: T => U) = StreamBuilder[I,U](initial, i => f(mapper(i)))

  def complete(sink: Sink[T]): Unit = {
    initial.setDownstream(new MappedSink(mapper, sink))
  }

}
object StreamBuilder {
  def apply[T](initial: LiveSink[T]): StreamBuilder[T, T] = StreamBuilder(initial, x => x)
}

case class ChainedStreamBuilder[X, T](previous: Stream[X], next: StreamBuilder[X, T]) extends Stream[T] {

  def map[U](f: T => U) = ChainedStreamBuilder(previous, next.map(f))

  def complete(sink: Sink[T]): Unit = {
    next.complete(sink)
    previous.complete(next.initial)
  }


}

trait Stream[T] {
  def map[U](f: T => U): Stream[U]

  def chain(otherSink: LiveSink[T]): Stream[T] = {
    ChainedStreamBuilder(this, StreamBuilder(otherSink))
  }
  
  def complete[E](collector: Deferred[Collector[T, E]]): Deferred[E] = collector.flatMap {c =>
    complete(c)
    ConstantDeferred(c.result)
  }

  def complete(sink: Sink[T]): Unit

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
          println("iter pushing")
          val res = downstream.get.push(iter.next)
          println(s"res $res")
          res match {
            case PushResult.Ok => {}
            case PushResult.WaitAccepted(signal) => {
              signal.onSignaled{
                continueDrain()
              }
              continue = false
            }
            case PushResult.WaitRejected(signal) => {
            }
            case PushResult.Error(err) => {
              continue = false
            }
          }
        }
        if (!iter.hasNext) {
          downstream.get.close()
        }
      }
    }
    InitialStreamBuilder(source)
  }

}


