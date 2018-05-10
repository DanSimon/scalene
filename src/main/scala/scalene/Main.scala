package scalene

import microactor.Pool

object Main extends App {

  implicit val p = new Pool

  val settings = ServerSettings(
    port = 9876,
    addresses = Nil,
    maxConnections = 100,
    tcpBacklogSize = None
  )

  Server.start(settings)

  p.join()

}
