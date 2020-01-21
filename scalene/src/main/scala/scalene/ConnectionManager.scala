package scalene

import scala.concurrent.duration._
import java.nio.channels.{SelectionKey, SocketChannel}
import java.net.InetSocketAddress

import util._

sealed trait ConnectionState

object ConnectionState {
  case object NotConnected extends ConnectionState
  case object Connecting   extends ConnectionState
  case object Connected    extends ConnectionState
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

  def lastActivity = math.max(lastWriteTime, lastReadTime)

}

trait ConnectionHandle extends ConnectionInfo {

  def time: TimeKeeper

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

  val time = channel.time
  val startTime = channel.time()
  private var writeOverflowBuffer: Option[ReadBuffer] = None

  private var _bytesRead      = 0L
  private var _bytesWritten   = 0L
  private var _lastReadTime   = time()
  private var _lastWriteTime  = time()
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

  def onInitialize(env: AsyncContext): Unit = handler.onInitialize(env)

  def onRead(buffer: ReadBuffer, wrt: ReadWriteBuffer): Unit = {
    _lastReadTime = channel.time()
    _bytesRead = buffer.size
    handler.onReadData(buffer)
    if (channel.writeReadyEnabled) {
      onWrite(wrt)
    }
  }

  def onWrite(buffer: ReadWriteBuffer): Unit = {
    _lastWriteTime = channel.time()
    writeOverflowBuffer match {
      case Some(overflow) => {
        _bytesWritten += channel.write(overflow)
        if (overflow.isEmpty) {
          writeOverflowBuffer = None
          //now that the overflow buffer is empty, give the handler a chance to
          //write something
          onWrite(buffer)
        }
      }
      case None => {
        handler.onWriteData(buffer)
        if (!buffer.isEmpty) {
          val readBuffer = buffer.data
          val written = channel.write(readBuffer)
          _bytesWritten += written
          if (!readBuffer.isEmpty) {
            writeOverflowBuffer = Some(readBuffer.readCopy)
          }
          //println(s"${written} written : ${buffer.isEmpty}")
        } else {
          channel.disableWriteReady()
        }
      }
    }
  }

  def onConnected() : Unit = handler.onConnected(this)

  def finishConnect() : Unit = {
    channel.finishConnect()
    onConnected()
  }

  def onDisconnected(reason: DisconnectReason) = handler.onDisconnected(reason)

}

