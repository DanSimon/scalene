package scalene

import microactor.Pool

object Main extends App {

  implicit val p = new Pool

  val settings = ServerSettings(
    port = 9876,
    addresses = Nil,
    maxConnections = 100,
    tcpBacklogSize = None,
    numWorkers = Some(2)
  )

  val factory: ConnectionContext => ServerConnectionHandler = ctx => new ServerConnectionHandler {

    def onInitialize() {
      println("initialize")
    }

    var temp = ""
    var _handle: Option[ConnectionHandle] = None

    def onReadData(buffer: ReadBuffer) {
      temp = new String(buffer.takeAll)
      _handle.get.requestWrite()
    }

    def onWriteData(buffer: WriteBuffer) {
      buffer.write(temp.getBytes())
    }

    def onConnected(handle: ConnectionHandle) {
      println("new connection")
      _handle = Some(handle)

    }

    def onDisconnected() {
      println("disconnected")
    }

  }

  Server.start(settings, factory, new RefreshOnDemandTimeKeeper(new RealTimeKeeper))

  p.join()

}
