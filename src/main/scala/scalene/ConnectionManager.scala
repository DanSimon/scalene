package scalene

import scala.concurrent.duration._
import java.nio.channels.{SelectionKey, SocketChannel}
import java.net.InetSocketAddress

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

class ConnectionManager(
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

  def disconnect() {
    //TODO: proper shutdown sequence
    channel.close()
  }

  def requestWrite() {
    channel.enableWriteReady()
  }

  def disableReads() {
    channel.disableReads()
  }

  def enableReads() {
    channel.enableReads
  }

  def remoteAddress: Option[java.net.InetSocketAddress] = channel.remoteAddress
  def state: scalene.ConnectionState = channel.state


  def timeIdle = channel.time() - math.max(lastReadTime, lastWriteTime)

  def onInitialize(): Unit = handler.onInitialize()

  def onRead(buffer: ReadBuffer): Unit = {
    _lastReadTime = channel.time()
    _bytesRead = buffer.size
    handler.onReadData(buffer)
  }

  def onWrite(buffer: ReadWriteBuffer): Unit = {
    _lastWriteTime = channel.time()
    handler.onWriteData(buffer)
    _bytesWritten = buffer.size
    //TODO : overflow
    channel.write(buffer.data)
    channel.disableWriteReady()
  }

  def onConnected() : Unit = handler.onConnected(this)

  def finishConnect() : Unit = {
    channel.finishConnect()
    onConnected()
  }

  def onDisconnected() = handler.onDisconnected()

}

