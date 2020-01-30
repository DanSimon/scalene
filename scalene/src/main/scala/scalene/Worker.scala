package scalene


import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, SocketChannel}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

import scalene.actor._
import util._


sealed trait WorkerMessage

sealed trait ServerToWorkerMessage extends WorkerMessage
object ServerToWorkerMessage {
  case class NewConnection(channel: SocketChannel) extends ServerToWorkerMessage
  case object Shutdown extends ServerToWorkerMessage
}



class ServerWorker(
  server: Actor[WorkerToServerMessage],
  handlerFactory: AsyncContext => ServerConnectionHandler,
  timeKeeper: TimeKeeper,
  context: Context
) extends Receiver[WorkerMessage](context) with Logging {
  implicit val d = context.dispatcher

  val closedRelay = SimpleReceiver[EventLoopEvent]{
    case EventLoopEvent.ConnectionClosed => server.send(WorkerToServerMessage.ConnectionClosed)
  }

  val eventLoop = new EventLoop(timeKeeper, closedRelay)

  def receive(message: WorkerMessage) = message match {
    case ServerToWorkerMessage.NewConnection(channel) => {
      eventLoop.attachConnection(channel, handlerFactory)
    }
    case ServerToWorkerMessage.Shutdown => {
      eventLoop.shutdown()
      self.stop()
    }
  }

}
