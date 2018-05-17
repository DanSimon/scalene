package scalene

import java.net.{InetSocketAddress, ServerSocket, StandardSocketOptions}
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import microactor._

case class ServerSettings(
  port: Int,
  addresses: Seq[String],
  maxConnections: Int,
  tcpBacklogSize: Option[Int],
  numWorkers: Option[Int]
)

trait ServerMessage

trait ExternalServerMessage extends ServerMessage
object ExternalServerMessage {
  case object Shutdown extends ExternalServerMessage
}
sealed trait WorkerToServerMessage extends ServerMessage

private[this] case object SelectNow extends ServerMessage

class Server(
  settings: ServerSettings,
  handlerFactory: ConnectionContext => ServerConnectionHandler,
  timeKeeper: TimeKeeper
) extends BasicReceiver[ServerMessage] with Logging {

  private var openConnections = 0

  val workers = collection.mutable.ArrayBuffer[Actor[ServerToWorkerMessage]]()

  class WorkerIterator extends Iterator[Actor[ServerToWorkerMessage]] {
    private var internal = workers.toIterator

    def hasNext = true

    def next = {
      if (!internal.hasNext) {
        internal = workers.toIterator
      }
      internal.next
    }
  }
  val workerIterator = new WorkerIterator

  val selector: Selector = Selector.open()
  val serverSocketChannel                = ServerSocketChannel.open()
  serverSocketChannel.configureBlocking(false)




  val ss: ServerSocket = serverSocketChannel.socket()
  val addresses: Seq[InetSocketAddress] =
    settings.addresses.isEmpty match {
      case true  => Seq(new InetSocketAddress(settings.port))
      case false => settings.addresses.map(address => new InetSocketAddress(address, settings.port))
    }

  serverSocketChannel.register(selector, SelectionKey.OP_ACCEPT)

  override def onStart(context: Context[ServerMessage]) {
    println("STARTING THE SERVER")
    super.onStart(context)
    (1 to settings.numWorkers.getOrElse(1)).foreach{i =>
      val dispatcher = context.dispatcher.pool.createDispatcher
      val actor = dispatcher.attach(new ServerWorker(
        context.self.specialize[WorkerToServerMessage],
        handlerFactory,
        timeKeeper
      )).specialize[ServerToWorkerMessage]
      workers.append(actor)

    }
    startServer()
    context.self.send(SelectNow)
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
          val serverSocketChannel: ServerSocketChannel = key.channel.asInstanceOf[ServerSocketChannel]
          val sc: SocketChannel        = serverSocketChannel.accept()
          if (openConnections < settings.maxConnections) {
            openConnections += 1
            sc.configureBlocking(false)
            sc.socket.setTcpNoDelay(true)
            //router ! Worker.NewConnection(sc)
            workerIterator.next.send(ServerToWorkerMessage.NewConnection(sc))            
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

  def start(settings: ServerSettings, factory: ConnectionContext => ServerConnectionHandler, timeKeeper: TimeKeeper)(implicit pool: Pool): Actor[ExternalServerMessage] = {
    val dispatcher = pool.createDispatcher
    dispatcher.attach(new Server(settings, factory, timeKeeper)).specialize[ExternalServerMessage]
  }

}


