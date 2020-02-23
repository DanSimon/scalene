package scalene

import java.util.concurrent.atomic.AtomicLong
import scala.util.{Failure, Success, Try}
import scala.concurrent.{Future, Promise}

import scalene.actor._
import util._

trait GenericUniqueKey

case class UniqueKey[T](id: Long) extends GenericUniqueKey

object UniqueKey {
  private val next = new AtomicLong(1)

  def generate[T]: UniqueKey[T] = UniqueKey[T](next.incrementAndGet())
}

class LocalizedImpl[T](key: UniqueKey[T], generator: AsyncContext => T) {
  def apply()(implicit context: AsyncContext) : T = context.localized(key)(generator(context))
}
object localized {
  def apply[T](generator: AsyncContext => T) = new LocalizedImpl(UniqueKey.generate[T], generator)
}


trait Deferred[T] {
  def resolve(context: AsyncContext): Async[T]

  def map[U](f: T => U): Deferred[U] 

  def flatMap[U](f: T => Deferred[U]): Deferred[U] 
}

case class ConstantAsyncDeferred[T](async: Async[T]) extends Deferred[T] {

  def resolve(context: AsyncContext): Async[T] = async

  def map[U](f: T => U): Deferred[U] = Deferred(async.map(f))

  def flatMap[U](f: T => Deferred[U]): Deferred[U] = defer(_ => async).flatMap(f)
}

case class ConstSuccessDeferred[T](value: T) extends Deferred[T] {

  def resolve(context: AsyncContext): Async[T] = Async.successful(value)

  def map[U](f: T => U): Deferred[U] = try {
    ConstSuccessDeferred(f(value))
  } catch {
    case e: Exception => Deferred.failure(e)
  }

  def flatMap[U](f: T => Deferred[U]): Deferred[U] = try {
    f(value)
  } catch {
    case e: Exception => Deferred.failure(e)
  }

}


class CapturedDeferred[T](cap: AsyncContext => Async[T]) extends Deferred[T] {

  def resolve(context: AsyncContext) = cap(context)

  def map[U](f: T => U): Deferred[U] = new CapturedDeferred(ctx => cap(ctx).map(f))

  def flatMap[U](f: T => Deferred[U]): Deferred[U] = new CapturedDeferred(ctx => cap(ctx).flatMap{t => f(t).resolve(ctx)})

}

object Deferred {
  def apply[T](async: Async[T]): Deferred[T] = ConstantAsyncDeferred(async)

  def successful[T](value: T): Deferred[T] = ConstSuccessDeferred(value)

  def failure[T](error: Throwable): Deferred[T] = Deferred(ConstantAsync(Failure(error)))

}

object defer {
  def apply[T](f: AsyncContext => Async[T]): Deferred[T] = new CapturedDeferred(f)
}

class DeferredClient[Request, Response](
  factory: AsyncContext => BasicClient[Request, Response],
  config: BasicClientConfig
) {

  val localClient = localized { ctx =>
    ctx.eventLoop.attachAndConnect(config.address, factory)
  }

  def send(request: Request): Deferred[Response] = defer { implicit ctx => 
    localClient().send(request)
  }  

}

class DeferredExecutor(implicit p: Pool) {
  implicit val dispatcher = p.createDispatcher("deferred-executor-{ID}")
  private val timeKeeper = new RealTimeKeeper

  private val receiver = SimpleReceiver[EventLoopEvent]{e => ()}

  private val eventLoop = new EventLoop(timeKeeper, receiver)

  trait GenericExecutor {
    def execute()
  }
  
  case class Executor[T](deferred: Deferred[T], promise: Promise[T]) extends GenericExecutor {
    def execute(): Unit = {
      deferred
        .resolve(eventLoop.environment)
        .onComplete{t => promise.complete(t)}

    }
  }

  private val sender = SimpleReceiver[GenericExecutor]{ex =>
    ex.execute()
  }

  def apply[T](deferred: Deferred[T]): Future[T] = {
    val p = Promise[T]()
    sender.send(Executor(deferred, p))
    p.future
  }
}


