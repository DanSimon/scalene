package scalene

import java.util.LinkedList
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

class ServiceServer[I,O](codecFactory: Codec.Factory[I,O], requestHandler: RequestHandler[I,O]) 
extends ServerConnectionHandler {

  private val codec = codecFactory(processRequest)

  private val pendingRequests = new LinkedList[Async[O]]

  final def processRequest(request: I): Unit = {
    val async = requestHandler.handleRequest(request)

    pendingRequests.add(async)
    if (pendingRequests.size == 1) {
      async.onComplete{_ => _handle.foreach{_.requestWrite()}}
    }
  }

  def onInitialize() {
  }

  var _handle: Option[ConnectionHandle] = None

  final def onReadData(buffer: ReadBuffer) {
    codec.decode(buffer)
  }

  final def onWriteData(buffer: WriteBuffer) = {
    while (!buffer.isOverflowed && pendingRequests.size > 0 && pendingRequests.peek.result.isDefined) {
      pendingRequests.remove.result.get match {
        case Success(response) => codec.encode(response, buffer)
        case Failure(err) => codec.encode(requestHandler.handleError(None, err), buffer)
      }
    }
    if (pendingRequests.size > 0) {
      if (pendingRequests.peek.result.isEmpty) {
        pendingRequests.peek.onComplete{_ => _handle.foreach{_.requestWrite()}}
        false
      } else {
        true
      }
    } else {
      false
    }
  }

  def onConnected(handle: ConnectionHandle) {
    _handle = Some(handle)

  }

  def onDisconnected(reason: DisconnectReason) {
    //println(s"disconnected: $reason")
  }

}
