
package scalene.actor

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.lang.Thread

/**
 * If a message mixes this in, sending it won't trigger the dispatcher's wake
 * locks.  This is only useful when an actor wants to send a message to itself
 * telling it to block.
 * */
trait NoWakeMessage

trait UntypedActorHandle {
  def kill(): Unit
  def stop(): Unit
  def id: Long
  def untypedReceiver: UntypedReceiver
  def dispatcher: Dispatcher
}

trait Actor[T] extends UntypedActorHandle {
  def send(message: T): Boolean

  def specialize[U <: T]: Actor[U] = this.asInstanceOf[Actor[U]]

}


sealed trait ActorState
object ActorState {
  case object Starting extends ActorState
  case object Alive extends ActorState
  case object Stopping extends ActorState
  case object Stopped extends ActorState
}

sealed trait ProcessResult
object ProcessResult {
  case object ProcessMore extends ProcessResult
  case object ProcessEnd extends ProcessResult
  case object ActorStopped extends ProcessResult
}

//this is the object put into the dispatcher's queue.

trait UntypedActorMessageProcessor {
  def process(): Unit
}

case class ActorMessageProcessor[T](actor: ActorHandle[T], message: T) extends UntypedActorMessageProcessor {
  def process(): Unit = {
    actor.receiver.receive(message)
  }
}

class ActorHandle[T](
  val dispatcher: DispatcherImpl,
  val receiverF: Context => Receiver[T],
  val id: Long
) extends Actor[T] with UntypedActorHandle {

  val context: Context = new Context(this.asInstanceOf[Actor[Any]], dispatcher)

  private var _state: ActorState = ActorState.Starting
  private var currentReceiver = receiverF(context)
  def receiver = currentReceiver

  def untypedReceiver = receiver

  private def restart(exception: Exception) {
    currentReceiver.onBeforeRestart(exception)
    currentReceiver = receiverF(context)
    currentReceiver.onStart()
    currentReceiver.onAfterRestart()
  }

  def start() {
    _state = ActorState.Alive
    receiver.onStart()
  }

  def stop() {
    _state = ActorState.Stopping
  }

  def send(message: T) : Boolean = if (_state == ActorState.Alive) {
    dispatcher.queueMessage(ActorMessageProcessor(this, message))
    true
  } else false

  def kill() = {
    _state = ActorState.Stopped
    receiver.onStop() //maybe indicate somehow that its being killed vs shutdown
  }
}


class Context private[actor](untypedself: Actor[Any], _dispatcher: Dispatcher) {
  private[actor] def retypeSelf[T] = untypedself.asInstanceOf[Actor[T]]

  implicit val dispatcher = _dispatcher
}

trait UntypedReceiver {
  def onStop(): Unit = {}

}

abstract class Receiver[T](context: Context) extends UntypedReceiver {

  val self = context.retypeSelf[T]
  val dispatcher = context.dispatcher

  def receive(message: T): Unit

  def onStart(): Unit = {}
  def onBeforeRestart(exception: Exception): Unit = {}
  def onAfterRestart(): Unit = {}

}


class Pool {
  
  val id = System.nanoTime

  private val nextId = new AtomicLong(0)

  private val dispatchers = new collection.mutable.Queue[DispatcherImpl]

  def createDispatcher(name: String) = synchronized {
    val id = nextId.incrementAndGet.toInt
    val fixedName = name.replace("{ID}", id.toString)
    val d = new DispatcherImpl(this, id, fixedName)
    dispatchers.enqueue(d)
    d
  }

  def shutdown(): Unit = synchronized {
    dispatchers.foreach{_.shutdown}
  }

  def join()  = synchronized {
    while (!dispatchers.isEmpty) {
      try {
        //need to keep the head in the queue until its dead
        dispatchers.head.thread.join
        dispatchers.dequeue
      } catch {
        case e: Exception => println(e.toString)
      }
    }
  }


}


class SimpleReceiver[T](val context: Context, val receiver: T => Unit) extends Receiver[T](context) {

  def receive(t: T): Unit = receiver(t)

}
object SimpleReceiver {
  def apply[T](r: T => Unit)(implicit dispatcher: Dispatcher): Actor[T] = dispatcher.attach(new SimpleReceiver(_, r))
}


