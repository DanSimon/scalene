package scalene


import java.net.InetSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.{SelectionKey, Selector, SocketChannel}

import scala.collection.JavaConverters._
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

class ConnectionContext

sealed trait WorkerMessage

private[this] case object Select extends WorkerMessage

trait ConnectionHandler {

  def onReadData(buffer: ReadBuffer)

  def onWriteData(buffer: WriteBuffer)

  def onConnected()

  def onConnectionTerminated()

}

class OutputManager

class ConnectionManager(val handler: ConnectionHandler) {



}

trait ServerConnectionHandler extends ConnectionHandler

class ServerWorker(
  server: Actor[WorkerToServerMessage],
  handlerFactory: ConnectionContext => ServerConnectionHandler)  extends BasicReceiver[WorkerMessage] with Logging {


  private val selector = Selector.open()

  private val readBuffer = ByteBuffer.allocateDirect(1024 * 128)

  private val writeBufferBuffer = new WriteBufferImpl(1024 * 1024 * 4)

  private val activeConnections = collection.mutable.Map[Long, ConnectionManager]()


  override def onStart(context: Context[WorkerMessage] {
    super.onStart(context)
    context.self.send(Select)
  }

  def receive(message: WorkerMessage) = message match {
    case Select => {
      select()
      context.send(Select)
    }
  }

  private def select() {
    selector.select(1) //need short wait times to register new connections
    implicit val TIME = System.currentTimeMillis
    val selectedKeys  = selector.selectedKeys()
    val it            = selectedKeys.iterator()
    while (it.hasNext) {
      val key: SelectionKey = it.next
      if (!key.isValid) {
        error("KEY IS INVALID")
      } else if (key.isConnectable) {
        val con = key.attachment.asInstanceOf[ClientConnection]
        try {
          con.handleConnected()
        } catch {
          case t: Throwable => {
            unregisterConnection(con, DisconnectCause.ConnectFailed(t))
            key.cancel()
          }
        }
      } else {
        if (key.isReadable) {
          // Read the data
          buffer.clear
          val sc: SocketChannel = key.channel().asInstanceOf[SocketChannel]
          try {
            val len = sc.read(buffer)
            if (len > -1) {
              key.attachment match {
                case connection: Connection => {
                  buffer.flip
                  val data = DataBuffer(buffer, len)
                  connection.handleRead(data)
                } //end case
              }
            } else {
              //reading -1 bytes means the connection has been closed
              key.attachment match {
                case c: Connection => {
                  unregisterConnection(c, DisconnectCause.Closed)
                  key.cancel()
                }
              }
            }
          } catch {
            case t: java.io.IOException => {
              key.attachment match {
                case c: Connection => {
                  //connection reset by peer, sometimes thrown by read when the connection closes
                  unregisterConnection(c, DisconnectCause.Closed)
                }
              }
              sc.close()
              key.cancel()
            }
            case t: Throwable => {
              warn(s"Unknown Error! : ${t.getClass.getName}: ${t.getMessage}")
              if (trace) {
                t.printStackTrace()
              }
              //close the connection to ensure it's not in an undefined state
              key.attachment match {
                case c: Connection => {
                  warn(s"closing connection ${c.id} due to unknown error")
                  unregisterConnection(c, DisconnectCause.Error(t))
                }
                case other => {
                  error(s"Key has bad attachment!! $other")
                }
              }
              sc.close()
              key.cancel()
            }
          }
        }
        //have to test for valid here since it could have just been cancelled above
        if (key.isValid && key.isWritable) {
          key.attachment match {
            case c: Connection =>
              try {
                c.handleWrite(outputBuffer)
                outputBuffer.reset()
              } catch {
                case j: java.io.IOException => {
                  unregisterConnection(c, DisconnectCause.Error(j))
                }
                case other: Throwable => {
                  warn(s"Error handling write: ${other.getClass.getName} : ${other.getMessage}")
                }
              }
            case _ => {}
          }
        }
      }
      it.remove()

    }

  }



}
