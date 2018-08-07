
package microactor

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
  def process(max: Int): ProcessResult
  def id: Long
  def untypedReceiver: UntypedReceiver
  def dispatcher: Dispatcher
}

trait Actor[T] extends UntypedActorHandle {
  def send(message: T): Boolean

  def specialize[U <: T]: Actor[U] = this.asInstanceOf[Actor[U]]

}
/*
object Actor {
  def empty[T]: Actor[T] = new Actor[T]{
    def send(message: T) = true
  }
}
*/


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

class ActorHandle[T](
  val dispatcher: DispatcherImpl,
  val receiverF: Context => Receiver[T],
  val id: Long
) extends Actor[T] with UntypedActorHandle {

  val context: Context = new Context(this.asInstanceOf[Actor[Any]], dispatcher)

  private val queue = new ConcurrentLinkedQueue[T]()
  private var _state: ActorState = ActorState.Starting
  private var currentReceiver = receiverF(context)
  def receiver = currentReceiver

  private val processing = new AtomicBoolean(false)

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
    //receiver.onStop()
    if (queue.isEmpty) {
      _state = ActorState.Stopped
      dispatcher.controller.send(ControlMessage.Stop(this))
    }
  }


  def send(message: T) : Boolean = if (_state == ActorState.Alive) {
    val toNotify = (queue.peek == null) 
    queue.add(message)
    if (toNotify) {
      if (!message.isInstanceOf[NoWakeMessage]) {
        dispatcher.wake()
      }
      dispatcher.notify(this)
    }
    true
  } else false

  /*
   * returns true if there's more work to do
   */
  def process(max: Int): ProcessResult = {
    processing.set(true)
    var num = 0
    while (num < max && !queue.isEmpty) {
      try {
        receiver.receive(queue.poll)
      } catch {
        case e: Exception => restart(e)
      }
    }
    processing.set(false)
    if (queue.isEmpty) {
      if (_state == ActorState.Stopping) {
        _state = ActorState.Stopped
        ProcessResult.ActorStopped
      } else {        
        ProcessResult.ProcessEnd
      }
      
    } else ProcessResult.ProcessMore
  }

  def kill() = {
    queue.clear()
    _state = ActorState.Stopped
    receiver.onStop() //maybe indicate somehow that its being killed vs shutdown
  }
}


class Context private[microactor](untypedself: Actor[Any], _dispatcher: Dispatcher) {
  private[microactor] def retypeSelf[T] = untypedself.asInstanceOf[Actor[T]]

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

  def createDispatcher = synchronized {
    val d = new DispatcherImpl(this, nextId.incrementAndGet.toInt)
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


