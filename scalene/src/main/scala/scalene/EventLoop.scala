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

sealed trait EventLoopEvent
object EventLoopEvent {
  case object ConnectionClosed extends EventLoopEvent
}

object Select extends NoWakeMessage

class EventLoop(
  timeKeeper: TimeKeeper,
  eventReceiver: Actor[EventLoopEvent]
)(implicit dispatcher: Dispatcher)  extends Logging {



  private val selector = Selector.open()

  private val readBuffer = ByteBuffer.allocateDirect(1024 * 12)

  private val writeBuffer = new WriteBufferImpl(1024 * 128)

  private val activeConnections = collection.mutable.Map[Long, ConnectionManager]()

  private val timer = new Timer(50)

  val environment = new AsyncContextImpl(timeKeeper, timer, dispatcher, this)

  private val selectActor: Actor[Select.type] = SimpleReceiver[Select.type]{_ => 
    processSelect()
    if (selector.selectNow() > 0) {
      selectActor.send(Select)
    } else {
      coSelect.send(Select)
    }
  }

  val selectDispatcher = dispatcher.pool.createDispatcher("selector-{ID}")
  //this is needed because if the worker sends Select to itself it doesn't
  //yield execution to other actors
  private val coSelect: Actor[Select.type] = {
    selectDispatcher.attach(new Receiver[Select.type](_) {
      def receive(m: Select.type) {
        selector.select()
        selectActor.send(Select)
      }
    })
  }

  def shutdown(): Unit = {
    selectDispatcher.shutdown()
    selectActor.stop()
    activeConnections.values.foreach{connection =>
      removeConnection(connection, DisconnectReason.Shutdown)
    }
  }

  selectActor.send(Select)
  private def scheduleIdleTimeout(): Unit = timer.schedule(1000){ 
    closeIdleConnections() 
    scheduleIdleTimeout()
  }
  scheduleIdleTimeout()

  private var _nextId = 0L
  private def nextId() = {
    _nextId += 1
    _nextId
  }

  private def attachInternal(channel: SocketChannel, handler: ConnectionHandler, immediatelyConnected: Boolean, keyInterest: Int): Unit = {
    selector.wakeup()
    val key = channel.register(selector, keyInterest)
    val handle = new LiveChannelHandle(channel, key, timeKeeper)
    val manager = new ConnectionManager(nextId(), handler, handle)
    key.attach(manager)
    activeConnections(manager.id) = manager
    manager.onInitialize(environment)
    if (immediatelyConnected) {
      manager.onConnected()
    }

  }

  def attachConnection[T <: ConnectionHandler](channel: SocketChannel, handlerF: AsyncContext => T): T = {
    val handler = handlerF(environment)
    attachInternal(channel, handler, true, SelectionKey.OP_READ)
    handler
  }

  /**
   * WARNING - not thread-safe.  Should only be called from within the context of the event loop's dispatcher
   */
  def attachAndConnect[T <: ConnectionHandler](address: InetSocketAddress, handlerF: AsyncContext => T): T = {
    val channel = SocketChannel.open()
    channel.configureBlocking(false)
    try {
      val handler = handlerF(environment)
      attachInternal(channel, handler, channel.connect(address), SelectionKey.OP_CONNECT | SelectionKey.OP_READ)
      handler
    } catch {
      case t: Throwable => {
        error(s"Failed to establish connection to $address: $t", t)
        ???
      }
    }
  }

  private def removeConnection(manager: ConnectionManager, reason: DisconnectReason): Unit = {
    manager.disconnect()
    activeConnections.remove(manager.id)
    manager.onDisconnected(reason)
    eventReceiver.send(EventLoopEvent.ConnectionClosed)
  }

  private def closeIdleConnections(): Unit = {
    timeKeeper.refresh()
    val toClose = activeConnections.filter{case (_, c) => 
      c.handler.idleTimeout.isFinite && c.lastActivity < (timeKeeper() - c.handler.idleTimeout.toMillis)
    }
    toClose.foreach{case (_, c) => 
      removeConnection(c, DisconnectReason.TimeOut)
    }    
    if (!toClose.isEmpty) {
      info(s"""closed ${toClose.size} idle connection${if (toClose.size > 1) "s" else ""}""")
    }
  }

  private def processSelect() {
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
            error(s"Error while connecting, $t")
            t.printStackTrace()
            removeConnection(con, DisconnectReason.ClientConnectFailed(t))
            key.cancel()
          }
        }
      } else {
        if (key.isReadable) {
          readBuffer.clear
          val sc: SocketChannel = key.channel().asInstanceOf[SocketChannel]
          val manager = key.attachment.asInstanceOf[ConnectionManager]
          try {
            val len = sc.read(readBuffer)
            if (len > -1) {
              readBuffer.flip
              val buffer = ReadBuffer(readBuffer, len)
              writeBuffer.reset()
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
              t.printStackTrace
              removeConnection(manager, DisconnectReason.Error(t))
              sc.close()
              key.cancel()
            }
          }
        }
        if (key.isValid && key.isWritable) {
          val manager = key.attachment.asInstanceOf[ConnectionManager]
          try {
                writeBuffer.reset()
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
