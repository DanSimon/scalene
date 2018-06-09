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

trait ConnectionContext {
  def time: TimeKeeper
}

sealed trait WorkerMessage

sealed trait ServerToWorkerMessage extends WorkerMessage
object ServerToWorkerMessage {
  case class NewConnection(channel: SocketChannel) extends ServerToWorkerMessage
}


private[this] case object Select extends WorkerMessage




trait ServerConnectionHandler extends ConnectionHandler

class ServerWorker(
  server: Actor[WorkerToServerMessage],
  handlerFactory: ConnectionContext => ServerConnectionHandler,
  timeKeeper: TimeKeeper
) extends BasicReceiver[WorkerMessage] with Logging {


  private val selector = Selector.open()

  private val readBuffer = ByteBuffer.allocateDirect(1024 * 128)

  private val writeBuffer = new WriteBufferImpl(1024 * 1024 * 4)

  private val activeConnections = collection.mutable.Map[Long, ConnectionManager]()

  private var _nextId = 0L
  private def nextId() = {
    _nextId += 1
    _nextId
  }

  override def onStart(context: Context[WorkerMessage]) {
    super.onStart(context)
    context.dispatcher.addWakeLock(new WakeLock {
      def wake(): Unit = {
        selector.wakeup()
      }
    })
    context.self.send(Select)
  }

  def receive(message: WorkerMessage) = message match {
    case Select => {
      select()
      context.self.send(Select)
    }
    case ServerToWorkerMessage.NewConnection(channel) => {
      val key = channel.register(selector, SelectionKey.OP_READ)
      val handle = new LiveChannelHandle(channel, key, timeKeeper)
      val context = new ConnectionContext {
        def time = timeKeeper
      }
      val manager = new ConnectionManager(nextId(), handlerFactory(context), handle)
      key.attach(manager)
      activeConnections(manager.id) = manager
      manager.onInitialize()
      manager.onConnected()
    }
  }

  private def removeConnection(manager: ConnectionManager, reason: DisconnectReason): Unit = {
    activeConnections.remove(manager.id)
    manager.onDisconnected(reason)
    server.send(WorkerToServerMessage.ConnectionClosed)
    server.dispatcher.wake()
  }

  private def select() {
    selector.select() //need short wait times to register new connections
    timeKeeper.refresh()
    val selectedKeys  = selector.selectedKeys.iterator
    while (selectedKeys.hasNext) {
      val key: SelectionKey = selectedKeys.next
      if (!key.isValid) {
        error("KEY IS INVALID")
      } else if (key.isConnectable) {
        val con = key.attachment.asInstanceOf[ConnectionManager]
        try {
          con.finishConnect()
        } catch {
          case t: Throwable => {
            removeConnection(con, DisconnectReason.ClientConnectFailed(t))
            key.cancel()
          }
        }
      } else {
        writeBuffer.reset()
        if (key.isReadable) {
          readBuffer.clear
          val sc: SocketChannel = key.channel().asInstanceOf[SocketChannel]
          val manager = key.attachment.asInstanceOf[ConnectionManager]
          try {
            val len = sc.read(readBuffer)
            if (len > -1) {
              readBuffer.flip
              val buffer = ReadBuffer(readBuffer, len)
              manager.onRead(buffer, writeBuffer)
            } else {
              removeConnection(manager, DisconnectReason.RemoteClosed)
              key.cancel()
            }
          } catch {
            case t: java.io.IOException => {
              removeConnection(manager, DisconnectReason.RemoteClosed)
              sc.close()
              key.cancel()
            }
            case t: Throwable => {
              warn(s"Unknown Error! : ${t.getClass.getName}: ${t.getMessage}")
              removeConnection(manager, DisconnectReason.Error(t))
              sc.close()
              key.cancel()
            }
          }
        }
        if (key.isValid && key.isWritable) {
          val manager = key.attachment.asInstanceOf[ConnectionManager]
          try {
                manager.onWrite(writeBuffer)
          } catch {
            case e: java.io.IOException => {
              removeConnection(manager, DisconnectReason.Error(e))
            }
          }
        }
      }
      selectedKeys.remove()

    }

  }

}
