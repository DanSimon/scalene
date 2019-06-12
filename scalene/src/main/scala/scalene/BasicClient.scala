package scalene

import util._
import java.net.InetSocketAddress
import java.util.LinkedList
import scala.concurrent.duration._

case class BasicClientConfig(
  address: InetSocketAddress,
  maxInFlightRequests: Int,
  maxPendingRequests: Int,
  pendingTimeout: Duration,
  inFlightTimeout: Duration,
  failFast: Boolean
)

object BasicClientConfig {
  def default(host: String, port: Int) = BasicClientConfig(
    address = new InetSocketAddress(host, port),
    maxInFlightRequests = 1000,
    maxPendingRequests = 1000,
    pendingTimeout = Duration.Inf,
    inFlightTimeout = Duration.Inf,
    failFast = false
  )
}



class ClientException(message: String) extends Exception(message)

class BasicClient[Request,Response](
  factory: Codec.Factory[Response,Request],
  config: BasicClientConfig,
  env: AsyncContext
) extends ConnectionHandler with Logging {

  case class PendingRequest(request: Request, promise: PromiseAsync[Response], createdTS: Long)

  val idleTimeout = Duration.Inf

  private val codec = factory(processResponse)
  private val pendingRequests = new LinkedList[PendingRequest]
  private val inFlightRequests = new LinkedList[PendingRequest]
  var _handle: Option[ConnectionHandle] = None


  def send(request: Request): Async[Response] = {
    def enqueue(): Async[Response] = {
      if (pendingRequests.size < config.maxPendingRequests) {
        val promise = new PromiseAsync[Response]
        if (pendingRequests.size == 0) {
          _handle.foreach{_.requestWrite()}
        }
        pendingRequests.add(PendingRequest(request, promise, env.time()))

        promise
      } else {
        Async.failure(new ClientException("Too many pending requests"))
      }
    }
    _handle match {
      case Some(han) => enqueue()
      case None => if (config.failFast) {
        Async.failure(new ClientException("not connected"))
      } else {
        enqueue()
      }
    }
  }

  final def processResponse(response: Response): Unit = {
    println(s"got response $response")
    val done = inFlightRequests.remove
    done.promise.succeed(response)

    if (pendingRequests.size > 0) {
      _handle.map{_.requestWrite()}
    }
  }

  def onInitialize(e: AsyncContext) {
  }


  final def onReadData(buffer: ReadBuffer) {
    codec.decode(buffer)
  }

  private def resetConnection(): Unit = {
    //kill the connection
    _handle.foreach{_.disconnect()}
  }

  
  final def onWriteData(buffer: WriteBuffer) = {
    while (!buffer.isOverflowed && pendingRequests.size > 0  && inFlightRequests.size <= config.maxInFlightRequests) {
      val p = pendingRequests.remove
      codec.encode(p.request, buffer)
      inFlightRequests.add(p)
    }
    if (pendingRequests.size > 0 && inFlightRequests.size <= config.maxInFlightRequests) {
      true
    } else {
      false
    }
  }

  def onConnected(handle: ConnectionHandle) {
    _handle = Some(handle)
    info("client connected")
    if (pendingRequests.size > 0) {
      handle.requestWrite()
    }
  }

  def onDisconnected(reason: DisconnectReason) {
    //println(s"disconnected: $reason")
    info("client disconnected")
    _handle = None
  }

}

import scalene.stream._

abstract class StreamOutputSink extends PartialSink[Writable] {

  private var buffer: Option[WriteBuffer] = None
  

  private val signal = new LiveSignal

  //these are implemented in the client when it extends as a nested class
  //def close(): Unit = {}
  //def error(reason: Throwable): Unit = {}

  //return true if the upstream has more to write
  def visit(_buffer: WriteBuffer): Boolean = {
    buffer = Some(_buffer)
    signal.signal()
    buffer = None
    //note - this may not be a surefire way to tell if there is more to write,
    //since the last push on the stream may overflow the buffer, but that
    //shouldn't be a big deal since in that case close() would be called
    //immediately after
    _buffer.isOverflowed
  }

  def attemptPush(writable: Writable): PartialPushResult = buffer match {
    case Some(buf) => {
      writable.writeTo(buf)
      if (buf.isOverflowed) {
        PushResult.WaitAccepted(signal)
      } else {
        PushResult.Ok
      }
    }
    case None => {
      PartialPushResult.WaitRejected(signal)
    }
  }

}

trait OutputManager[T] {

  private var liveOutputStream: Option[StreamOutputSink] = None

  protected def hasNextOutputItem(): Boolean
  protected def nextOutputItem(): T
  protected def encoder: MessageEncoder[T]
  protected def onOutputError(reason: Throwable): Unit

  final def onWriteData(buffer: WriteBuffer) = {
    while (liveOutputStream.isEmpty && !buffer.isOverflowed && hasNextOutputItem()) {
      encoder.encode(nextOutputItem(), buffer) match {
        case None => {}
        case Some(streamBuilder) => {
          val sink = new StreamOutputSink with BufferedSink[Writable] {

            override def onClose(): Unit = {
              liveOutputStream = None
            }
            override def onError(reason: Throwable) : Unit = {
              onOutputError(reason)
            }
          }            
          streamBuilder.complete(sink)
          liveOutputStream = Some(sink)
        }
      }
    }
    //return true if we detect there is more immediately available to write
    liveOutputStream match {
      case Some(sink) => {
        sink.visit(buffer)
      }
      case None => hasNextOutputItem()
    }
  }


}


/*

class DataOutputSink(handle: ConnectionHandle, maxBufferSize: Int) extends Sink[RawData] {

  private var buffer: Option[WriteBuffer] = None

  def visit(_buffer: WriteBuffer): Unit = {
    buffer = Some(_buffer)
    signal.signal()
    buffer = None
  }

  def push(data: RawData): PushResult = 

}

*/
