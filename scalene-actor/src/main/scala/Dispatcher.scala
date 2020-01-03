package scalene.actor

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, LinkedBlockingQueue}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.lang.Thread

sealed trait ControlMessage
case object ControlMessage {
  case class Stop(actor: UntypedActorHandle) extends ControlMessage
  case object Shutdown extends ControlMessage
}

trait Dispatcher {
  def shutdown(): Unit
  def pool: Pool

  def id: Int
  def attach[T](actorFactory: Context => Receiver[T]): Actor[T]
}

class DispatcherImpl(val pool: Pool, val id: Int, val name: String) extends Dispatcher {

  private val nextId = new AtomicLong()
  private val actors = new ConcurrentHashMap[Long, UntypedActorHandle]()

  private val messageQueue = new LinkedBlockingQueue[UntypedActorMessageProcessor]()

  def queueMessage(message: UntypedActorMessageProcessor): Unit = {
    messageQueue.add(message)
  }

  private val running = new AtomicBoolean(true)

  val thread = new Looper
  thread.setDaemon(false)
  thread.start
  val controller = attach(new Controller(_))
  val threadId = thread.getId

  def shutdown() {
    running.set(false)
    controller.send(ControlMessage.Shutdown)
  }

  private def stopActor(actor: UntypedActorHandle) {
    actors.remove(actor.id)
    actor.untypedReceiver.onStop()
  }

  private def finishShutdown(): Unit = {
    val it = actors.values.iterator
    while (it.hasNext) {
      it.next.kill()
    }
    actors.clear()
  }

  class Controller(context: Context) extends Receiver[ControlMessage](context) {
    def receive(m: ControlMessage) = m match {
      case ControlMessage.Stop(actor) => stopActor(actor)
      case ControlMessage.Shutdown => {
        running.set(false)
        finishShutdown()
      }
    }
  }

  class Looper extends Thread(name) {

    val lock = new Object

    override def run() : Unit = {
      while (running.get()) {
        try {
          messageQueue.take().process()
        } catch {
          case e: InterruptedException => {}
          case e: Exception => {
            running.set(false)
          }
        }
      }
      //terminate all attached actors
      val it = actors.values.iterator
      while (it.hasNext) {
        it.next.kill()
      }

    }
  }

  def attach[T](receiver: Context => Receiver[T]): Actor[T] = {
    val actor = new ActorHandle[T](this, receiver, nextId.incrementAndGet)
    actors.put(actor.id, actor)
    actor.start()
    actor
  }

}
