package scalene


import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, SocketChannel}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

import microactor._

class ConnectionContext

sealed trait WorkerMessage

sealed trait ServerToWorkerMessage extends WorkerMessage
object ServerToWorkerMessage {
  case class NewConnection(channel: SocketChannel) extends ServerToWorkerMessage
}


private[this] case object Select extends WorkerMessage



trait ConnectionHandler {

  def onInitialize()

  def onReadData(buffer: ReadBuffer)

  def onWriteData(buffer: WriteBuffer)

  def onConnected(handle: ConnectionHandle)

  def onDisconnected()

}

trait ServerConnectionHandler extends ConnectionHandler

class ServerWorker(
  server: Actor[WorkerToServerMessage],
  handlerFactory: ConnectionContext => ServerConnectionHandler,
  timeKeeper: TimeKeeper
) extends BasicReceiver[WorkerMessage] with Logging {

  println("new worker")


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
      val manager = new ConnectionManager(nextId(), handlerFactory(new ConnectionContext), handle)
      activeConnections(manager.id) = manager
    }
  }

  private def select() {
    selector.select(1) //need short wait times to register new connections
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
            //unregisterConnection(con, DisconnectCause.ConnectFailed(t))
            key.cancel()
          }
        }
      } else {
        if (key.isReadable) {
          readBuffer.clear
          val sc: SocketChannel = key.channel().asInstanceOf[SocketChannel]
          try {
            val len = sc.read(readBuffer)
            if (len > -1) {
              readBuffer.flip
              val buffer = ReadBuffer(readBuffer, len)
              key.attachment.asInstanceOf[ConnectionManager].onRead(buffer)
            } else {
              //unregisterConnection(c, DisconnectCause.Closed)
              key.cancel()
            }
          } catch {
            case t: java.io.IOException => {
                  //unregisterConnection(c, DisconnectCause.Closed)
              sc.close()
              key.cancel()
            }
            case t: Throwable => {
              warn(s"Unknown Error! : ${t.getClass.getName}: ${t.getMessage}")
              /*
                  warn(s"closing connection ${c.id} due to unknown error")
                  unregisterConnection(c, DisconnectCause.Error(t))
                  */
              sc.close()
              key.cancel()
            }
          }
        }
        //have to test for valid here since it could have just been cancelled above
        if (key.isValid && key.isWritable) {
          val manager = key.attachment.asInstanceOf[ConnectionManager]
          try {
                manager.onWrite(writeBuffer)
          } catch {
            case j: java.io.IOException => {
              //unregisterConnection(c, DisconnectCause.Error(j))
            }
          }
          writeBuffer.reset()
        }
      }
      selectedKeys.remove()

    }

  }



}
