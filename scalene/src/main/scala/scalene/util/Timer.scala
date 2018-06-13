package scalene.util

import java.util.{LinkedList, TreeSet}
import microactor._



trait Event {
  def cancel()
}
class EventTrigger(trigger: () => Unit) extends Event {
  private var cancelled = false
  def cancel() {
    cancelled = true
  }
  def execute(): Unit = if (!cancelled) {
    trigger()
  }
}


class Timer(resolutionMillis: Int = 10)(implicit dispatcher: Dispatcher) {

  private val executions = collection.mutable.Map[Long, LinkedList[EventTrigger]]()
  private var lastExecutedHash = 0L

  val jtimer = new java.util.Timer

  sealed trait TimerExecutorCommand
  object TimerExecutorCommand {
    case class Execute(hash: Long) extends TimerExecutorCommand
  }
  class TimerExecutor(ctx: Context) extends Receiver[TimerExecutorCommand](ctx) {

    def receive(m: TimerExecutorCommand): Unit = m match {
      case TimerExecutorCommand.Execute(hash) => {
        //println("got the tick")
        if (executions.contains(hash)) {
          val events = executions(hash)
          executions.remove(hash)
          while (events.size > 0) {
            events.remove().execute()
          }
          lastExecutedHash = hash
        }
      }
    }

    override def onStop() {
      jtimer.cancel()
    }
  }

  private val executor = dispatcher.attach(new TimerExecutor(_))

  def schedule(inMillis: Long)(event: => Unit) = {
    val hash:Long = ((System.currentTimeMillis + inMillis).toDouble / resolutionMillis).toLong * resolutionMillis
    //println(s"now ${System.currentTimeMillis} -> $hash")
    if (!executions.contains(hash)) {
      executions(hash) = new LinkedList[EventTrigger]
      val task = new java.util.TimerTask {
        def run(): Unit = {
          //println("tick")
          executor.send(TimerExecutorCommand.Execute(hash))
        }
      }
      jtimer.schedule(task, inMillis)
    }
    val trigger = new EventTrigger(() => event)
    executions(hash).add(trigger)
    trigger
  }

}



