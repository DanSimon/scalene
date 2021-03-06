package scalene

import java.nio.channels.{SelectionKey, SocketChannel}
import java.net.InetSocketAddress

import util._

trait ChannelHandle {
  def close() 
  def finishConnect() 

  def host : InetSocketAddress

  def write(raw: ReadBuffer): Int

  protected def keyInterestOps(ops: Int) 

  def remoteAddress: Option[InetSocketAddress]

  def time: TimeKeeper

  def state: ConnectionState


  private var _readsEnabled      = true
  private var _writeReadyEnabled = false

  def readsEnabled      = _readsEnabled
  def writeReadyEnabled = _writeReadyEnabled

  protected def setKeyInterest() {
    val read = if (readsEnabled) SelectionKey.OP_READ else 0
    val write = if (writeReadyEnabled) SelectionKey.OP_WRITE else 0
    val ops =  read | write 
    keyInterestOps(ops)
  }

  def enableReads() {
    _readsEnabled = true
    setKeyInterest()
  }
  def disableReads() {
    _readsEnabled = false
    setKeyInterest()
  }
  def enableWriteReady(): Unit = if (! _writeReadyEnabled) {
    _writeReadyEnabled = true
    setKeyInterest()
  }

  def disableWriteReady(): Unit = if (_writeReadyEnabled) {
    _writeReadyEnabled = false
    setKeyInterest()
  }

}

class LiveChannelHandle(
  val channel: SocketChannel,
  val selectionKey: SelectionKey,
  val time: TimeKeeper)
extends ChannelHandle {
  def close() {
    channel.close()
  }
  def finishConnect() {
    channel.finishConnect()
  }

  def write(raw: ReadBuffer): Int = raw.writeTo(channel)

  protected def keyInterestOps(ops: Int) { 
    selectionKey.interestOps(ops)
    //changing key ops requires waking up the selector if it's blocked
    selectionKey.selector.wakeup()
  }

  def remoteAddress: Option[InetSocketAddress] = try {
    Some(channel.getRemoteAddress.asInstanceOf[InetSocketAddress])
  } catch {
    case t: Throwable => None
  }
  

  lazy val host: InetSocketAddress = 
    channel.socket.getInetAddress.asInstanceOf[InetSocketAddress]

  def port: Int = try {
    channel.socket.getPort
  } catch {
    case n: NullPointerException => 0
  }

  def state = if (channel.isConnectionPending) {
    ConnectionState.Connecting
  } else if (channel.isConnected) {
    ConnectionState.Connected
  } else {
    ConnectionState.NotConnected
  }
  
}
