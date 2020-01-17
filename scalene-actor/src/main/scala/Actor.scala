
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
  def start(): Unit
  def stop(): Unit
  def id: Long
  def untypedReceiver: UntypedReceiver
  def dispatcher: Dispatcher
  def restart(exception: Exception): Unit
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

sealed trait DispatcherMessage

trait UntypedActorMessageProcessor extends DispatcherMessage{
  def actor: UntypedActorHandle
  def process(): Unit
}

case class ActorMessageProcessor[T](actor: ActorHandle[T], message: T) extends UntypedActorMessageProcessor {
  def process(): Unit = {
    actor.receiver.receive(message)
  }
}

sealed trait ActorMetaAction
object ActorMetaAction {
  case object StopActor extends ActorMetaAction
}

trait UntypedAttachActorMessage extends DispatcherMessage {
  def actor: UntypedActorHandle
}

case class AttachActorMessage[T](actor: Actor[T]) extends UntypedAttachActorMessage

case class ActorMetaActionMessage(actor: UntypedActorHandle, action: ActorMetaAction) extends DispatcherMessage
case object ShutdownDispatcher extends DispatcherMessage

class ActorHandle[T](
  val dispatcher: DispatcherImpl,
  val receiverF: Context => Receiver[T],
  val id: Long
) extends Actor[T] with UntypedActorHandle {

  val context: Context = new Context(this.asInstanceOf[Actor[Any]], dispatcher)

  private var _state: ActorState = ActorState.Starting
  private var _sendable: Boolean = true
  private var currentReceiver = receiverF(context)
  def receiver = currentReceiver

  def untypedReceiver = receiver

  def restart(exception: Exception) {
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
    _sendable = false
    dispatcher.queueMessage(ActorMetaActionMessage(this, ActorMetaAction.StopActor))
  }

  def send(message: T) : Boolean = if (_sendable) {
    dispatcher.queueMessage(ActorMessageProcessor(this, message))
    true
  } else false

  def kill() = {
    _state = ActorState.Stopped
    _sendable = false
    receiver.onStop() //maybe indicate somehow that its being killed vs shutdown
  }
}

