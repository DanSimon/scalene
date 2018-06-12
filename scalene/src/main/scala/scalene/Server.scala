package scalene

import java.net.{InetSocketAddress, ServerSocket, StandardSocketOptions}
import java.nio.channels.{SelectionKey, Selector, ServerSocketChannel, SocketChannel}
import scala.concurrent.duration.Duration
import microactor._
import util._

case class ServerSettings(
  port: Int,
  addresses: Seq[String],
  maxConnections: Int,
  tcpBacklogSize: Option[Int],
  numWorkers: Option[Int],
  maxIdleTime: Duration
)

sealed trait ServerMessage

trait ExternalServerMessage extends ServerMessage
object ExternalServerMessage {
  case object Shutdown extends ExternalServerMessage
}

sealed trait WorkerToServerMessage extends ServerMessage
object WorkerToServerMessage {
  case object ConnectionClosed extends WorkerToServerMessage
}

private[this] case object SelectNow extends ServerMessage

class ServerActor(
  settings: ServerSettings,
  handlerFactory: ConnectionContext => ServerConnectionHandler,
  timeKeeper: TimeKeeper,
  context: Context
) extends Receiver[ServerMessage](context) with Logging {

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

  override def onStart() {
    super.onStart()
    (1 to settings.numWorkers.getOrElse(1)).foreach{i =>
      val dispatcher = context.dispatcher.pool.createDispatcher
      val actor: Actor[ServerToWorkerMessage] = dispatcher.attach(ctx => new ServerWorker(
        self.specialize[WorkerToServerMessage],
        handlerFactory,
        timeKeeper,
        settings.maxIdleTime,
        ctx
      )).specialize[ServerToWorkerMessage]
      workers.append(actor)

    }
    startServer()
    context.dispatcher.addWakeLock(new WakeLock {
      def wake(): Unit = {
        selector.wakeup()
      }
    })
    self.send(SelectNow)
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
      self.send(SelectNow)
    }
    case WorkerToServerMessage.ConnectionClosed => {
      openConnections -= 1
    }
  }

  private def select(): Unit = {
    selector.select()
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
            val w = workerIterator.next
            w.send(ServerToWorkerMessage.NewConnection(sc))
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

  type Server = Actor[ExternalServerMessage]

  def start(settings: ServerSettings, factory: ConnectionContext => ServerConnectionHandler, timeKeeper: TimeKeeper)(implicit pool: Pool): Actor[ExternalServerMessage] = {
    val dispatcher = pool.createDispatcher
    dispatcher.attach(ctx => new ServerActor(settings, factory, timeKeeper, ctx)).specialize[ExternalServerMessage]
  }

}


