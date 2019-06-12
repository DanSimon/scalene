package scalene
package stream

trait Signal {
  def onSignaled(f: => Unit): Unit
  def isSignaled: Boolean
  def isFilled : Boolean
}

class LiveSignal extends Signal {
  private var event: Option[() => Unit] = None
  private var signaled = false
  def onSignaled(f: => Unit): Unit = if (signaled) {
    f
    signaled = false
  } else {
    event = Some(() => f)
  }

  def signal(): Unit = {
    event match {
      case  Some(f) =>  { 
        f()
        event = None
      }
      case None => {
        signaled = true
      }
    }
  }

  def reset(): Unit = {
    signaled = false
    event = None
  }

  def isSignaled = signaled
  def isFilled = event.isDefined
}

sealed trait PartialPushResult
object PartialPushResult {
  //the item was not pushed due to backpressure.
  case class WaitRejected(signal: Signal) extends PartialPushResult
}

trait PartialSink[T] {
  def attemptPush(item: T): PartialPushResult
}
sealed trait PushResult extends PartialPushResult
object PushResult {
  case object Ok extends PushResult

  //the item was pushed, but backpressure has been signaled, subsequent items may cause an error
  case class WaitAccepted(signal: Signal) extends PushResult

  case class Error(reason: Throwable) extends PushResult
}

trait Receiver[T] {
  def push(item: T): PushResult
}

/*
 * Trait for all stream-related methods that aren't directly about streaming items
 */
trait Transport {
  def close(): Unit = onClose()
  def error(reason: Throwable): Unit = onError(reason)

  protected def onClose(): Unit
  protected def onError(reason: Throwable): Unit
}

trait Sink[T] extends Receiver[T] with Transport

trait Collector[T, E] extends Sink[T]{
  def result: Async[E]
}

object Sink {

  def blackHole[T]: Sink[T] = new Sink[T] {
    def push(item: T) = PushResult.Ok
    def onClose() = {}
    def onError(reason: Throwable) = {}
  }
}

trait BufferedSink[T] extends Sink[T] with PartialSink[T] {

  private val buffer = new java.util.LinkedList[T]
  private val externalSignal = new LiveSignal
  private var closed = false

  def maxBufferSize: Int = 1000

  override def push(item: T) = if (closed) {
    PushResult.Error(new Exception("cannot  push to closed sink"))
  } else if (buffer.size > 0) {
    buffer.add(item)
    PushResult.WaitAccepted(externalSignal)
  } else {
    val res = attemptPush(item) 
    //println(s"buffered attempt push got $res")
    res match {
      case PartialPushResult.WaitRejected(signal) => {
        buffer.add(item)
        signal.onSignaled{
          drainBuffer()
        }
        PushResult.WaitAccepted(externalSignal)
      }
      case other: PushResult => other
    }
  }

  abstract override def close(): Unit = if (!closed) {
    closed = true
    if (buffer.size == 0) {
      super.close()
    }
  }

  private def drainBuffer() {
    var continue = true
    while (continue && buffer.size > 0) {
      attemptPush(buffer.remove()) match {
        case PushResult.Ok => {}
        case PushResult.WaitAccepted(signal) => {
          signal.onSignaled {
            drainBuffer()
          }
          continue = false
        }
        case PartialPushResult.WaitRejected(_) => {
          //this should not happen
          throw new Exception("rejected push from buffer drain")
        }
        case PushResult.Error(err) => {
          continue = false
        }
      }
    }
    if (continue && buffer.size == 0) {
      if (closed) {
        super.close()
      } else {
        externalSignal.signal()
      }
    }
  }       

}



//class GluedSink[T](from: Sink[T], to: Sink[T]) extends Sink[T]

//trait for an object that will produce items that should be pushed to a downstream Sink.
trait LiveSource[T] {
  protected var downstream: Option[Sink[T]] = None
  protected val setSignal = new LiveSignal

  def setDownstream(sink: Sink[T]): Unit = {
    //println(s"$this setting downstream to $sink")
    downstream = Some(sink)
    setSignal.signal()
  }

  def setBlackHoleIfUnset(): Unit = {
    //println("setting to black hole")
    if (downstream.isEmpty) {
      setDownstream(Sink.blackHole[T])
    }
  }

}

class LiveSink[T] extends Sink[T] with LiveSource[T]{

  def push(item: T) = downstream
    .map{_.push(item)}
    .getOrElse(PushResult.Error(new Exception("Attempt to push to incomplete sink")))

  override def onClose() = downstream.foreach{_.close()}

  override def onError(reason: Throwable) = downstream.foreach{_.error(reason)}
}

class MappedSink[A,B](mapper: A => B, downstream: Sink[B]) extends Sink[A] {

  def push(item: A) = downstream.push(mapper(item))

  override def onClose() = downstream.close()

  override def onError(reason: Throwable): Unit = downstream.error(reason)
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
  
  def complete[E](collector: Collector[T, E]): Deferred[E] = defer {c =>
    complete(collector: Sink[T])
    collector.result
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
          val res = downstream.get.push(iter.next)
          res match {
            case PushResult.Ok => {}
            case PushResult.WaitAccepted(signal) => {
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
        if (!iter.hasNext) {
          downstream.get.close()
        }
      }
    }
    InitialStreamBuilder(source)
  }

}


