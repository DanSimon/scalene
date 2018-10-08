package scalene
package stream

sealed trait PushResult
object PushResult {
  case object Ok extends PushResult
  case class Wait(signal: Signal) extends PushResult
  case class Error(reason: Throwable) extends PushResult
}


trait Collector[T, E] extends Sink[T]{
  def result: Async[E]
}

trait Sink[T] {
  def push(item: T): PushResult
  //close the stream, triggering the result
  def close(): Unit  
  def error(reason: Throwable): Unit
}

class LiveSink[T] extends Sink[T] {
  private var downstream: Option[Sink[T]] = None

  def setDownstream(sink: Sink[T]): Unit = {
    downstream = Some(sink)
  }

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

class StreamBuilder[I, T](initial: LiveSink[I], mapper: I => T) extends Stream[T] {

  def map[U](f: T => U) = new StreamBuilder[I,U](initial, i => f(mapper(i)))

  def complete[E](collector: Deferred[Collector[T, E]]): Deferred[E] = collector.flatMap {c =>
    initial.setDownstream(new MappedSink(mapper, c))
    defer(c.result)
  }

}


trait Stream[T] {
  def map[U](f: T => U): Stream[T]

  def complete[E](collector: Deferred[Collector[T, E]]): Deferred[E]

}
