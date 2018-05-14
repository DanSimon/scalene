package scalane

import scala.concurrent.duration._
import java.nio.channels.{SelectionKey, SocketChannel}
import java.net.InetSocketAddress
import server._

/**
  * Represent the connection state.  NotConnected, Connected or Connecting.
  */
sealed trait ConnectionState

object ConnectionState {
  case object NotConnected extends ConnectionState
  case object Connected    extends ConnectionState
  case object Connecting   extends ConnectionState
}

trait ConnectionInfo {

  def state: ConnectionState
  def id: Long
  def lastWriteTime: Long
  def lastReadTime: Long
  def bytesWritten: Long
  def bytesRead: Long
  def timeOpen: Long
  def timeIdle: Long
  def remoteAddress: Option[InetSocketAddress]

}

trait ConnectionHandle extends ConnectionInfo {

  def disconnect()
  def requestWrite()
  def disableReads()
  def enableReads()

}

trait ChannelHandle {
  def close() 
  def finishConnect() 

  def host() = channel.socket.getInetAddress

  def write(raw: ReadBuffer): Int

  protected def keyInterestOps(ops: Int) 

  override def remoteAddress: Option[InetSocketAddress]

  def time: TimeKeeper

}

abstract class ConnectionManager(
  val id: Long,
  val handler: ConnectionHandler,
  val channel: ChannelHandle)
  extends ConnectionHandle {

  val startTime = channel.time()

  private var _bytesRead      = 0L
  private var _bytesWritten   = 0L
  private var _lastReadTime   = 0L
  private var _lastWriteTime  = 0L
  def bytesRead      = _bytesRead
  def bytesWritten   = _bytesWritten
  def lastReadTime   = _lastReadTime
  def lastWriteTime  = _lastWriteTime

  def timeOpen = channel.time() - startTime


  def timeIdle = channel.time() - math.max(lastTimeDataReceived, lastTimeDataWritten)

  def handleRead(data: DataBuffer)(implicit time: Long) = {
    _lastTimeDataReceived = time
    myBytesReceived += data.size
    handler.receivedData(data)
  }
  def onInitialize(): Unit = handler.onInitialize()

  def onReadData(buffer: ReadBuffer): Unit = {
    _lastReadTime = channel.time()
    _bytesRead = buffer.size
    handler.onReadData(buffer)
  }

  def onWriteData(buffer: WriteBuffer): Unit = {
    _lastWriteTime = channel.time()
    handler.onWriteData(buffer)
    _bytesWritten = buffer.size
    //TODO : overflow
    channel.write(buffer.data)
  }

  def onConnected(handle: ConnectionHandle) : Unit = handler.onConnected(handle)

  def onDisconnected() = handler.onDisconnected()


}

/**
  * This mixin is used with all real connections, not in tests
  */
private[core] trait LiveConnection extends ChannelActions { self: Connection =>

  protected def channel: SocketChannel
  def key: SelectionKey

  def channelClose() { channel.close() }
  def finishConnect() { channel.finishConnect() } //maybe move into a subtrait extending it

  def channelHost() = channel.socket.getInetAddress

  def channelWrite(raw: DataBuffer): Int = raw.writeTo(channel)

  def keyInterestOps(ops: Int) {
    key.interestOps(ops)
  }

  override def remoteAddress: Option[InetSocketAddress] =
    try {
      Some(channel.getRemoteAddress.asInstanceOf[InetSocketAddress])
    } catch {
      case t: Throwable => None
    }

  //dont make these vals, doesn't work with client connections
  lazy val host: String = try {
    channel.socket.getInetAddress.getHostName
  } catch {
    case n: NullPointerException => "[Disconnected]"
  }
  def port: Int =
    try {
      channel.socket.getPort
    } catch {
      case n: NullPointerException => 0
    }

  def status =
    if (channel.isConnectionPending) {
      ConnectionState.Connecting
    } else if (channel.isConnected) {
      ConnectionState.Connected
    } else {
      ConnectionState.NotConnected
    }

}



