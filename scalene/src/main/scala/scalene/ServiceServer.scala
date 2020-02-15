package scalene

import java.util.LinkedList
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}


class ServiceServer[I,O](
  codecFactory: Codec.Factory[I,O],
  requestHandler: RequestHandler[I,O],
  val idleTimeout: Duration
) extends ServerConnectionHandler with OutputManager[O]{

  private val codec = codecFactory(processRequest)

  private val pendingRequests = new LinkedList[Async[O]]

  private var tempWrite: Option[WriteBuffer] = None

  final def processRequest(request: I): Unit = {
    val async = try {
      requestHandler.handleRequest(request)
    } catch {
      case e: Exception => ConstantAsync(Success(requestHandler.handleError(Some(request), e)))
    }
    if (!isStreaming && pendingRequests.isEmpty && async.result.isDefined && tempWrite.isDefined) {
      val response = async.result.get match {
        case Success(value) => value
        case Failure(ex) => requestHandler.handleError(None, ex)
      }
      writeItem(response, tempWrite.get)
    } else {
      pendingRequests.add(async)
      if (pendingRequests.size == 1) {
        async.onComplete{_ => _handle.foreach{_.requestWrite()}}
      }
    }
  }

  def onInitialize(env: AsyncContext) {
    //note - request handler is initialized on onConnected
  }


  var _handle: Option[ConnectionHandle] = None

  final def onReadData(buffer: ReadBuffer, writeOpt: Option[WriteBuffer]) {
    tempWrite = writeOpt
    codec.decode(buffer)
    tempWrite = None
  }


  def encoder = codec

  protected def hasNextOutputItem(): Boolean = {
    pendingRequests.size > 0 && pendingRequests.peek.result.isDefined
  }
  protected def nextOutputItem(): O = {
    //TODO, failure should include request
    pendingRequests.remove.result.get match {
      case Success(value) => value
      case Failure(ex) => requestHandler.handleError(None, ex)
    }
  }

  protected def onOutputError(reason: Throwable): Unit = {
    _handle.foreach{_.disconnect()}
  }

  def onConnected(handle: ConnectionHandle) {
    _handle = Some(handle)
    val rctx = new RequestHandlerContext {
      def time = handle.time
      def closeConnection(): Unit = {
        handle.disconnect()
      }
    }
    requestHandler.onInitialize(rctx)

  }

  def onDisconnected(reason: DisconnectReason) {
    //println(s"disconnected: $reason")
  }

}
