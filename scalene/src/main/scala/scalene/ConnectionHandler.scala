package scalene

import scala.concurrent.duration._

trait ConnectionHandler {

  def onInitialize(env: AsyncContext)

  def onReadData(buffer: ReadBuffer)

  def onWriteData(buffer: WriteBuffer): Boolean

  def onConnected(handle: ConnectionHandle)

  def onDisconnected(reason: DisconnectReason)

  def idleTimeout: Duration

}
