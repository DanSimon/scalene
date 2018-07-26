package scalene


import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, SocketChannel}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

import microactor._
import util._


sealed trait WorkerMessage

sealed trait ServerToWorkerMessage extends WorkerMessage
object ServerToWorkerMessage {
  case class NewConnection(channel: SocketChannel) extends ServerToWorkerMessage
}


case class WorkEnv(
  time: TimeKeeper,
  timer: Timer,
  dispatcher: Dispatcher
)


trait ServerConnectionHandler extends ConnectionHandler

class ServerWorker(
  server: Actor[WorkerToServerMessage],
  handlerFactory: WorkEnv => ServerConnectionHandler,
  timeKeeper: TimeKeeper,
  idleTimeout: Duration,
  context: Context
) extends Receiver[WorkerMessage](context) with Logging {
  implicit val d = context.dispatcher

  val closedRelay = SimpleReceiver[EventLoopEvent]{
    case EventLoopEvent.ConnectionClosed => server.send(WorkerToServerMessage.ConnectionClosed)
  }

  val eventLoop = new EventLoop(timeKeeper, idleTimeout, closedRelay)

  def receive(message: WorkerMessage) = message match {
    case ServerToWorkerMessage.NewConnection(channel) => {
      eventLoop.attachConnection(channel, handlerFactory)
    }
  }

}
