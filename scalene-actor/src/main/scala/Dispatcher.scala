package scalene.actor

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue, LinkedBlockingQueue}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.lang.Thread

trait Dispatcher {
  def shutdown(): Unit
  def pool: Pool

  def id: Int
  def attach[T](actorFactory: Context => Receiver[T]): Actor[T]
}

class DispatcherImpl(val pool: Pool, val id: Int, val name: String) extends Dispatcher {

  private val nextId = new AtomicLong()
  private val actors = new ConcurrentHashMap[Long, UntypedActorHandle]()

  private val messageQueue = new LinkedBlockingQueue[DispatcherMessage]()

  def queueMessage(message: DispatcherMessage): Unit = {
    messageQueue.add(message)
  }

  def nextActorId: Long = nextId.incrementAndGet

  private val running = new AtomicBoolean(true)

  val thread = new Looper
  thread.setDaemon(false)
  thread.start
  val threadId = thread.getId

  def shutdown() {
    running.set(false)
    thread.interrupt()
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

  private def processDispatcherMessage(d: DispatcherMessage): Unit = d match {

    case p: UntypedActorMessageProcessor => try {
      p.process()
    } catch {
      case e: Exception => {
        p.actor.restart(e)
      }
    }
    case a: ActorMetaActionMessage => a.action match {
      case ActorMetaAction.StopActor => stopActor(a.actor)
    }
    case ShutdownDispatcher => {
      running.set(false)
    }
    case a: UntypedAttachActorMessage => {
      val actor = a.actor
      actors.put(actor.id, actor)
      actor.start()
    }
    case ExecuteMessage(ex) => ex()
  }

  class Looper extends Thread(name) {

    val busyWaitMillis = 2
    var busyWaitStart = System.currentTimeMillis

    val lock = new Object


    override def run() : Unit = {
      while (running.get()) {
        try {
          val message = if (messageQueue.isEmpty){
            val busyStart = System.currentTimeMillis
            var gotSomething = false
            while (!gotSomething && System.currentTimeMillis - busyStart < busyWaitMillis) {
              val m = messageQueue.poll()
              //Thread.onSpinWait()
              if (m != null) {
                gotSomething = true
                processDispatcherMessage(m)
              }
            }
            if (!gotSomething) {
              processDispatcherMessage(messageQueue.take())
            }
          } else {
            processDispatcherMessage(messageQueue.poll())
          }
        } catch {
          case e: InterruptedException => {}
          case e: Exception => {
            println(e.toString)
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
    val actor = new ActorHandle[T](this, receiver, this.nextActorId) 
    queueMessage(AttachActorMessage(actor))
    //notice that we return the actor before it's attached and started, but that's ok because the attach message will arrive
    //before any user-sent messages
    actor
  }

  def execute(f: => Unit): Unit = {
    queueMessage(ExecuteMessage(() => f))
  }

}
