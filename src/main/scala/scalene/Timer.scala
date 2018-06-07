package scalene

import microactor._

sealed trait TimerControlCommand

object TimerControlCommand {
  case class Schedule(hash: Long) extends TimerControlCommand
}

/**
 * A timer requires two components.  A controller lives in a separate thread and sends messages to the receiver
 */

class TimerContoller(resolutionMillis: Int = 10, executor: Actor[ControllerToExecutor]) extends BasicReceiver[TimerControlCommand] {

  val hashes = new TreeSet[Long]()

  def checkAndPause(): Unit = {
    val now  = System.currentTimeMillis
    while (hashes.first < now) {

    }

  def receive(m: TimerControlCommand): Unit = m match {
    case Schedule(hash) => {
      hashes.add(hash) 
      checkAndPause()
    }

}

sealed trait TimerExecutorCommand

sealed trait ControllerToExecutor extends TimerExecutorCommand
object ControllerToExecutor {
  case class Execute(hash) extends ControllerToExecutor
}

class TimerExecutor(executions: collection.mutable.Map[Long, LinkedList[EventTrigger]])
extends BasicReceiver[TimerExecutorCommand] {

}

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

  val executions = collection.mutable.Map[Long, LinkedList[

  val executor = dispatcher.attach(new TimeExecutor(executions))

  val controllerDispatcher = dispatcher.pool.createDispatcher
  val controller = controllerDispatcher.attach(new TimerController(resolutionMillis, executor.specialize[ControllerToExecutor]))

  def schedule(inMillis: Long)(event: => Unit) = {
    val hash = (System.currentTimeMillis + inMillis) % resolutionMillis
    if (!executions.contains(hash)) {
      executions(hash) = new LinkedList[EventTrigger]
      executor.send(Schedule(hash))
    }
    val trigger = new EventTrigger(() => event)
    executions(hash).add(trigger)
    trigger
  }

}



