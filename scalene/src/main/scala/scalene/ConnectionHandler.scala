package scalene

trait ConnectionHandler {

  def onInitialize()

  def onReadData(buffer: ReadBuffer)

  def onWriteData(buffer: WriteBuffer): Boolean

  def onConnected(handle: ConnectionHandle)

  def onDisconnected(reason: DisconnectReason)

}
