package scalene

trait ConnectionHandler {

  def onInitialize(env: WorkEnv)

  def onReadData(buffer: ReadBuffer)

  def onWriteData(buffer: WriteBuffer): Boolean

  def onConnected(handle: ConnectionHandle)

  def onDisconnected(reason: DisconnectReason)

}
