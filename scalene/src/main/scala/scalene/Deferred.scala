package scalene

import java.util.concurrent.atomic.AtomicLong

trait GenericUniqueKey

case class UniqueKey[T](id: Long) extends GenericUniqueKey

object UniqueKey {
  private val next = new AtomicLong(1)

  def generate[T]: UniqueKey[T] = UniqueKey[T](next.incrementAndGet())
}

class Localized[T](key: UniqueKey[T], generator: AsyncContext => T) {
  def apply()(implicit context: AsyncContext) : T = context.localized(key)(generator(context))
}
object localized {
  def apply[T](generator: AsyncContext => T) = new Localized(UniqueKey.generate[T], generator)
}


trait Deferred[T] {
  def resolve(context: AsyncContext): Async[T]

  def map[U](f: T => U): Deferred[U] 

  def flatMap[U](f: T => Deferred[U]): Deferred[U] 
}

case class ConstantDeferred[T](async: Async[T]) extends Deferred[T] {

  def resolve(context: AsyncContext): Async[T] = async

  def map[U](f: T => U): Deferred[U] = ConstantDeferred(async.map(f))

  def flatMap[U](f: T => Deferred[U]): Deferred[U] = defer(_ => async).flatMap(f)
}

class CapturedDeferred[T](cap: AsyncContext => Async[T]) extends Deferred[T] {

  def resolve(context: AsyncContext) = cap(context)

  def map[U](f: T => U): Deferred[U] = new CapturedDeferred(ctx => cap(ctx).map(f))

  def flatMap[U](f: T => Deferred[U]): Deferred[U] = new CapturedDeferred(ctx => cap(ctx).flatMap{t => f(t).resolve(ctx)})

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
