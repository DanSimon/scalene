package scalene
package http

import util._
import Method._

import scalene.actor._
import scalene.routing._
import org.scalatest._
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import BasicConversions._

class ServerTest extends FlatSpec with Matchers with BeforeAndAfterAll {

  behavior of "Server"
  
  val settings = ServerSettings(
    port = 9876,
    addresses = Nil,
    maxConnections = 4096,
    tcpBacklogSize = None,
    numWorkers = Some(1)
  )

  class TestHandler extends ServerConnectionHandler {
    def onInitialize(env: AsyncContext){}
    def onReadData(buffer: ReadBuffer){}
    def onWriteData(buffer: WriteBuffer): Boolean = false
    def onConnected(handle: ConnectionHandle){}
    def onDisconnected(reason: DisconnectReason){}
    def idleTimeout: Duration = Duration.Inf
  }

  def withPool[T](f: Pool => T): T = {
    val p = new Pool
    val res = f(p)
    p.shutdown
    p.join
    res
  }

  it should "start and stop" in withPool {implicit p =>
    val server = Server.start(settings, s => new TestHandler, new RealTimeKeeper)
    server.blockUntilReady(500)
    server.shutdown()
    server.blockUntilShutdown(500)
  }

}


