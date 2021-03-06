package scalene.actor

import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}

//TODO: make more thread-safe!!

class Pool {
  
  val id = System.nanoTime

  private val nextId = new AtomicLong(0)

  private val dispatchers = new collection.mutable.Queue[DispatcherImpl]

  def createDispatcher(name: String) = {
    val id = nextId.incrementAndGet.toInt
    val fixedName = name.replace("{ID}", id.toString)
    val d = new DispatcherImpl(this, id, fixedName)
    synchronized {
      dispatchers.enqueue(d)
    }
    d
  }

  def shutdown(): Unit = synchronized {
    dispatchers.foreach{_.shutdown}
  }

  def join()  = {
    while (!dispatchers.isEmpty) {
      try {
        //need to keep the head in the queue until its dead
        dispatchers.head.thread.join
        synchronized {
          dispatchers.dequeue
        }
      } catch {
        case e: Exception => println(e.toString)
      }
    }
  }


}
