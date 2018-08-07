package scalene.actor

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.lang.Thread

sealed trait ControlMessage
case object ControlMessage {
  case class Stop(actor: UntypedActorHandle) extends ControlMessage
  case object Shutdown extends ControlMessage
}

//if an actor is using a blocking resource that can be woken externally (like
//an NIO selector), this can be used
trait WakeLock {
  def wake()
}


trait Dispatcher {
  def shutdown(): Unit
  def pool: Pool

  def id: Int
  def addWakeLock(lock: WakeLock)
  def attach[T](actorFactory: Context => Receiver[T]): Actor[T]
}

class DispatcherImpl(val pool: Pool, val id: Int) extends Dispatcher {

  private val nextId = new AtomicLong()
  private val actors = new ConcurrentHashMap[Long, UntypedActorHandle]()

  private val actorsWithWork = new ConcurrentLinkedQueue[UntypedActorHandle]()

  private val running = new AtomicBoolean(true)

  /*
   * This is the definitive truth for how many actors have messages to process.
   * This avoids race conditions where the dispatcher is notified of an actor
   * with work to do but the actor itself isn't yet added to actorsWithWork
   */
  private val numActive = new AtomicLong(0L)

  private val wakeLocks = new collection.mutable.ArrayBuffer[WakeLock]()

  def addWakeLock(lock: WakeLock) = synchronized {
    wakeLocks.append(lock)
  }

  def wake(): Unit = {
    wakeLocks.foreach{_.wake()}
  }

  val max = 100

  val thread = new Looper
  thread.setDaemon(false)
  thread.start
  val controller = attach(new Controller(_))
  val threadId = thread.getId

  def shutdown() {
    running.set(false)
    controller.send(ControlMessage.Shutdown)
    thread.lock.synchronized {
      thread.lock.notify()
    }
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
    actorsWithWork.clear()
    numActive.set(0)
  }

  class Controller(context: Context) extends Receiver[ControlMessage](context) {
    def receive(m: ControlMessage) = m match {
      case ControlMessage.Stop(actor) => stopActor(actor)
      case ControlMessage.Shutdown => {
        running.set(false)
        finishShutdown()
        wake()
      }
    }
  }

  

  class Looper extends Thread {

    val lock = new Object

    override def run() : Unit = {
      while (running.get()) {
        var pollAttempts = 0
        var continue = true
        while (continue) {
          val actor = actorsWithWork.poll
          //its possible we polled too early
          if (actor == null) {
            pollAttempts += 1
            if (pollAttempts == 50) {
              continue = numActive.get() > 0
              pollAttempts = 0
            }
          } else actor.process(max) match {
            case ProcessResult.ProcessMore => {
              actorsWithWork.add(actor)
            }
            case ProcessResult.ProcessEnd => {
              continue = numActive.decrementAndGet() > 0
            }
            case ProcessResult.ActorStopped => {
              stopActor(actor)
              continue = numActive.decrementAndGet() > 0
            }
          }
        }
        if (running.get()) {
          lock.synchronized {
            try {
              if (numActive.get() == 0) lock.wait()
            } catch {
              case e: InterruptedException => {
                running.set(false)
              }
              case e: Exception => {
                //the world is ending!
                running.set(false)
              }
            }
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

  def notify(notifier: UntypedActorHandle) =  {
    actorsWithWork.add(notifier)
    val num = numActive.incrementAndGet()
    thread.lock.synchronized {
      thread.lock.notifyAll()
    }
    //}
  }

}
