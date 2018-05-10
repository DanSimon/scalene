package scalene

import java.net.{InetSocketAddress, ServerSocket, StandardSocketOptions}
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import microactor._

case class ServerSettings(
  port: Int,
  addresses: Seq[String],
  maxConnections: Int,
  tcpBacklogSize: Option[Int]
)

trait ServerMessage

trait ExternalServerMessage extends ServerMessage
object ExternalServerMessage {
  case object Shutdown extends ExternalServerMessage
}

private[this] case object SelectNow extends ServerMessage

class Server(settings: ServerSettings) extends BasicReceiver[ServerMessage] with Logging {

  private var openConnections = 0

  val selector: Selector = Selector.open()
  val ssc                = ServerSocketChannel.open()
  ssc.configureBlocking(false)


  val ss: ServerSocket = ssc.socket()
  val addresses: Seq[InetSocketAddress] =
    settings.addresses.isEmpty match {
      case true  => Seq(new InetSocketAddress(settings.port))
      case false => settings.addresses.map(address => new InetSocketAddress(address, settings.port))
    }

  ssc.register(selector, SelectionKey.OP_ACCEPT)

  override def onStart(context: Context[ServerMessage]) {
    super.onStart(context)
    startServer()
  }

  def startServer() = {
    //setup the server
    try {
      addresses.foreach(address => ss.bind(address, settings.tcpBacklogSize.getOrElse(0)))
      info(s"name: Bound to ${addresses.mkString(", ")}")
      true
    } catch {
      case t: Throwable => {
        error(s"bind failed: ${t.getMessage}, retrying", t)
        false
      }
    }
  }

  def receive(s: ServerMessage) : Unit = s match {
    case SelectNow => {
      select()
    }
  }

  private def select(): Unit = {
    selector.select(5)
    val selectedKeys = selector.selectedKeys()
    val it           = selectedKeys.iterator()

    while (it.hasNext) {
      val key: SelectionKey = it.next
      if (!key.isValid) {
        it.remove()
      } else if (key.isAcceptable) {
        // Accept the new connection
        try {
          val ssc: ServerSocketChannel = key.channel.asInstanceOf[ServerSocketChannel]
          val sc: SocketChannel        = ssc.accept()
          if (openConnections < settings.maxConnections) {
            openConnections += 1
            sc.configureBlocking(false)
            sc.socket.setTcpNoDelay(true)
            //router ! Worker.NewConnection(sc)
            sc.close()
          } else {
            sc.close()
          }
        } catch {
          case c: java.nio.channels.NotYetBoundException => error("Attempted to accept before bound!?", c)
          case t: Throwable =>
            error(s"Error accepting connection: ${t.getClass.getName} - ${t.getMessage}", t)
        }
        it.remove()
      }
    }

  }

}

object Server {

  def start(settings: ServerSettings)(implicit pool: Pool): Actor[ExternalServerMessage] = {
    val dispatcher = pool.createDispatcher
    dispatcher.attach(new Server(settings)).specialize[ExternalServerMessage]
  }

}


