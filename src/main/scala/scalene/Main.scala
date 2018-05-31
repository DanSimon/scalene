package scalene

import java.nio.ByteBuffer
import java.util.{Arrays, LinkedList}
import microactor.Pool

object Main extends App {

  implicit val p = new Pool

  val settings = ServerSettings(
    port = 9876,
    addresses = Nil,
    maxConnections = 500,
    tcpBacklogSize = None,
    numWorkers = Some(1)
  )


  class RawCodec(onDecode: String => Unit) extends Codec[String, String](onDecode) {
    def decode(buf: ReadBuffer): Unit = {
      onDecode(new String(buf.takeAll))
    }

    def encode(message: String, buffer: WriteBuffer) : Unit = {
      buffer.write(message.getBytes)
    }

    def endOfStream = None
  }

  val dateHeader = new DateHeader//("Date", "Mon, 21 May 2018 05:21:58 GMT")
  val serverHeader = Header("Server", "benchmark")
  val contenttypeHeader = Header("Content-Type", "text/plain")
  val headers = Array(dateHeader, serverHeader, contenttypeHeader)
  val body = "Hello, World!".getBytes
  val plaintextUrl = "/plaintext".getBytes

  val firstLineMatch = s"${Method.Get.name.toUpperCase} /plaintext HTTP/1.1".getBytes

  val g = new ThreadLocal[String] {
    override def initialValue() = "wat"
  }

  val ref = new java.util.concurrent.atomic.AtomicReference[String]("hello")

  def handler = new RequestHandler[BasicHttpRequest, BasicHttpResponse] {
    var _context: Option[RequestHandlerContext] = None

    def handleRequest(input: BasicHttpRequest) = if (Arrays.equals(input.firstLine, firstLineMatch)){ //(input.fastMethodUrl(Method.Get, plaintextUrl)) {
      val t: Long = _context.get.time()
      //val y: String = g.get
      val y = ref.get()
      Async.successful(BasicHttpResponse(ResponseCode.Ok, headers, body))
    } else {
      Async.successful(BasicHttpResponse(ResponseCode.NotFound, headers, s"Unknown path".getBytes))
    }


    def handleError(req: Option[BasicHttpRequest], reason: Throwable) = BasicHttpResponse(
      ResponseCode.Error,
      Nil.toArray,
      reason.getMessage.getBytes
    )

    override def onInitialize(ctx: RequestHandlerContext): Unit = {
      _context = Some(ctx)
    }

  }

    val factory: ConnectionContext => ServerConnectionHandler = ctx => new ServiceServer((x: BasicHttpRequest => Unit) => new BasicHttpCodec(x, ctx.time), handler)

  Server.start(settings, factory, new RefreshOnDemandTimeKeeper(new RealTimeKeeper))

  p.join()

}
