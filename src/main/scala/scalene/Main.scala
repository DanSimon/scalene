package scalene

import java.util.Arrays
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

  val SPACE_BYTE = ' '.toByte

  class Method(val name: String) {
    val bytes = name.getBytes
  }
  
  object Method {
    val Get   = new Method("get")
    val Post  = new Method("post")
  }

  case class BasicHttpRequest(firstLine: Array[Byte], headers: Array[Array[Byte]], body: Array[Byte]) {
    def urlEquals(url: Array[Byte]): Boolean = {
      val sp = {
        var i = 3
        while (firstLine(i) != SPACE_BYTE) { i += 1 }
        i + 1
      }
      if (firstLine.length - sp < url.length || firstLine(sp + url.length) != SPACE_BYTE) {
        false
      } else {
        val m = Arrays.mismatch(firstLine, sp, sp + url.length, url, 0, url.length)
        m == -1
      }
    }

    def methodEquals(method: Method): Boolean = ParsingUtils.caseInsensitiveSubstringMatch(firstLine, method.bytes)
  }


  val p0 = "HTTP/1.1 ".getBytes
  val p1 = " OK\r\nContent-Length: ".getBytes
  val Newline = "\r\n".getBytes
  case class BasicHttpResponse(code: Int, headers: Array[Array[Byte]], body: Array[Byte]) {
    def encode(buffer: WriteBuffer) {
      buffer.write(p0)
      buffer.write(code)
      buffer.write(p1)
      buffer.write(body.size)
      buffer.write(Newline)
      var i = 0
      while (i < headers.length) {
        buffer.write(headers(i))
        i += 1
      }
      buffer.write(Newline)
      buffer.write(body)
    }
  }

  class BasicHttpCodec(onDecode: BasicHttpRequest => Unit) extends Codec[BasicHttpRequest, BasicHttpResponse](onDecode) {

    val zeroFirstLine = new Array[Byte](0)

    var buildFirstLine = new Array[Byte](0)
    var buildHeaders = new java.util.LinkedList[Array[Byte]]
    var buildContentLength = 0

    def getHeaders: Array[Array[Byte]] = {
      val arr = new Array[Array[Byte]](buildHeaders.size)
      var i = 0
      while (buildHeaders.size > 0) {
        arr(i) = buildHeaders.remove
        i+=1
      }
      arr
    }

    val contentLengthKey = "content-length".getBytes

    def body(buf: ReadBuffer): Unit = {
      onDecode(BasicHttpRequest(buildFirstLine,  getHeaders, buf.takeAll))
      buildHeaders.clear
      buildFirstLine = zeroFirstLine
      buildContentLength = 0
    }

    def line(buf: ReadBuffer): Int = {
      //println("line")
      if (buf.size == 0) { //0 size is the blank newline at the end of the head
        buildContentLength
      } else {
        if (buildFirstLine.length == 0) {
          buildFirstLine = buf.takeAll
        } else {          
          val header = buf.takeAll
          headerContentLength(header)
          buildHeaders.add(header)
        }
        BodyCode.HEAD_CONTINUE
      }
    }

    protected def headerContentLength(header: Array[Byte]): Unit = {
      if (buildContentLength == 0 && ParsingUtils.caseInsensitiveSubstringMatch(header, contentLengthKey)) {
        buildContentLength = trimStringToInt(header, contentLengthKey.length + 2)
        println(s"got content length $buildContentLength")
      }
    }


    private def trimStringToInt(line: Array[Byte], paddedStartIndex: Int): Int = {
      var i = line.length - 1
      var build = 0
      var mult = 1
      while (i >= paddedStartIndex) {
        println(line(i).toChar)
        if (line(i) != ' '.toByte) {
          build += (line(i) - 48) * mult
          mult *= 10
        }
        i -= 1
      }
      build
    }


    val parser = new HttpParser(line, body)

    def decode(buf: ReadBuffer): Unit = {
      parser.parse(buf)
    }

    def encode(message: BasicHttpResponse, buffer: WriteBuffer) {
      message.encode(buffer)
    }

    def endOfStream() {}

  }

  class RawCodec(onDecode: String => Unit) extends Codec[String, String](onDecode) {
    def decode(buf: ReadBuffer): Unit = {
      onDecode(new String(buf.takeAll))
    }

    def encode(message: String, buffer: WriteBuffer) : Unit = {
      buffer.write(message.getBytes)
    }

    def endOfStream = None
  }

  val dateHeader = "Date: Mon, 21 May 2018 05:21:58 GMT\r\n".getBytes
  val serverHeader = "Server: benchmark\r\n".getBytes
  val contenttypeHeader = "Content-Type: text/plain\r\n".getBytes
  val headers = Array(dateHeader, serverHeader, contenttypeHeader)
  val body = "Hello, World!".getBytes
  val plaintextUrl = "/plaintext".getBytes

  def handler = new RequestHandler[BasicHttpRequest, BasicHttpResponse] {
    def handleRequest(input: BasicHttpRequest) = if (input.methodEquals(Method.Get) && input.urlEquals(plaintextUrl)) {
      Async.successful(BasicHttpResponse(200, headers, body))
    } else {
      Async.successful(BasicHttpResponse(404, headers, s"Unknown path".getBytes))
    }


    def handleError(req: Option[BasicHttpRequest], reason: Throwable) = BasicHttpResponse(
      500,
      Nil.toArray,
      reason.getMessage.getBytes
    )

  }

  val factory: ConnectionContext => ServerConnectionHandler = ctx => new ServiceServer(new BasicHttpCodec(_), handler)

  Server.start(settings, factory, new RefreshOnDemandTimeKeeper(new RealTimeKeeper))

  p.join()

}
