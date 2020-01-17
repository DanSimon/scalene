package scalene.actor

class Context private[actor](untypedself: Actor[Any], _dispatcher: Dispatcher) {
  private[actor] def retypeSelf[T] = untypedself.asInstanceOf[Actor[T]]

  implicit val dispatcher = _dispatcher
}

trait UntypedReceiver {
  def onStop(): Unit = {}
  def onStart(): Unit = {}
  def onBeforeRestart(exception: Exception): Unit = {}
  def onAfterRestart(): Unit = {}

}

abstract class Receiver[T](context: Context) extends UntypedReceiver {

  val self = context.retypeSelf[T]
  val dispatcher = context.dispatcher

  def receive(message: T): Unit

}




class SimpleReceiver[T](val context: Context, val receiver: T => Unit) extends Receiver[T](context) {

  def receive(t: T): Unit = receiver(t)

}
object SimpleReceiver {
  def apply[T](r: T => Unit)(implicit dispatcher: Dispatcher): Actor[T] = dispatcher.attach(new SimpleReceiver(_, r))
}
