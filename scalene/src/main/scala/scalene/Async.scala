package scalene

import java.util.LinkedList
import scala.util.{Failure, Success, Try}

trait Async[T] {
  def map[U](f: T => U): Async[U]

  def flatMap[U](f: T => Async[U]): Async[U]

  def onComplete(cb: Try[T] => Unit): Unit

  def result: Option[Try[T]]

}

case class ConstantAsync[T](value: Try[T]) extends Async[T] {

  def map[U](f: T => U): Async[U] = ???

  def flatMap[U](f: T => Async[U]): Async[U] = ???

  def onComplete(cb: Try[T] => Unit): Unit = cb(value)

  val result = Some(value)

}

class PromiseAsync[T] extends Async[T] {

  def complete(result: Try[T]) : Unit = {
    value = Some(result)
    while (callbacks.size > 0) {
      val cb = callbacks.remove
      cb(result)
    }
  }

  def succeed(result: T): Unit = complete(Success(result))

  def fail(error: Throwable): Unit = complete(Failure(error))

  private var value: Option[Try[T]] = None

  def result = value

  private val callbacks = new LinkedList[Try[T] => Unit]()

  def map[U](f: T => U): Async[U] = value match {
    case Some(v) => ConstantAsync(v.map(f))
    case None => {
      val mapped = new PromiseAsync[U]
      callbacks.add(v => mapped.complete(v.map(f)))
      mapped
    }
  }

  def flatMap[U](f: T => Async[U]): Async[U] = value match {
    case Some(v) => v match {
      case Success(s) => f(s)
      case Failure(err) => ConstantAsync(Failure(err))
    }
    case None => {
      val mapped = new PromiseAsync[U]
      callbacks.add(v => v match {
        case Success(s) => f(s).onComplete(mapped.complete)
        case Failure(err) => mapped.complete(Failure(err))
      })
      mapped
    }
  }

  def onComplete(cb: Try[T] => Unit): Unit = value match {
    case Some(v) => cb(v)
    case None => callbacks.add(cb)
  }

}

object Async {

  def successful[T](value: T): Async[T] = ConstantAsync(Success(value))

  def failure[T](error: Throwable): Async[T] = ConstantAsync(Failure(error))

}
