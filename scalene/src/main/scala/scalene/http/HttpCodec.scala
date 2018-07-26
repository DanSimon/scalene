package scalene.http

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.{Arrays, Date, LinkedList, Locale, TimeZone}
import scalene._
import scalene.util._


object HttpParsing {
  val SPACE_BYTE = ' '.toByte
  val CODE_START = HttpVersion.`1.1`.stringValue.length + 1

  
  val ContentLengthPrefix = "Content-Length: ".getBytes
  val Newline = "\r\n".getBytes

}
import HttpParsing._

trait HttpMessageDecoder { self: FastArrayBuilding =>

  def finishDecode(firstLine: Array[Byte], headers: LinkedList[Header], body: Array[Byte])

  final val zeroFirstLine = new Array[Byte](0)
  final val contentLengthKey = "content-length".getBytes

  private var buildFirstLine = new Array[Byte](0)
  private var buildHeaders = new LinkedList[Header]
  private var buildContentLength = 0

  final def body(buf: ReadBuffer): Unit = {
    finishDecode(buildFirstLine,  buildHeaders, buf.readAll)
    buildHeaders = new LinkedList[Header]
    buildFirstLine = zeroFirstLine
    buildContentLength = 0
  }

  final def line(buf: ReadBuffer): Int = {
    if (buf.size == 0) { //0 size is the blank newline at the end of the head
      buildContentLength
    } else {
      if (buildFirstLine.length == 0) {
        buildFirstLine = buf.readAll
      } else {          
        val header = buf.readAll
        headerContentLength(header)
        buildHeaders.add(new StaticHeader(header))
      }
      BodyCode.HEAD_CONTINUE
    }
  }

  final protected def headerContentLength(header: Array[Byte]): Unit = {
    if (buildContentLength == 0 && ParsingUtils.caseInsensitiveSubstringMatch(header, contentLengthKey)) {
      buildContentLength = trimStringToInt(header, contentLengthKey.length + 2)
    }
  }


  final private def trimStringToInt(line: Array[Byte], paddedStartIndex: Int): Int = {
    var i = line.length - 1
    var build = 0
    var mult = 1
    while (i >= paddedStartIndex) {
      if (line(i) != ' '.toByte) {
        build += (line(i) - 48) * mult
        mult *= 10
      }
      i -= 1
    }
    build
  }

  def endOfStream() {}

  //parsing stuff
  private var parsingHead = true
  private var bodySize = 0

  final val headParser = new LineParser(line, false, 100)
  final val zeroBody = ReadBuffer(ByteBuffer.wrap(new Array[Byte](0)))

  def initSize = 1024
  def shrinkOnComplete = true

  final def decode(buffer: ReadBuffer): Unit = {
    while (parsingHead && buffer.hasNext) {
      bodySize = headParser.parse(buffer)
      if (bodySize == 0) {
        body(zeroBody)        
      } else if (bodySize > 0) {
        parsingHead = false        
      }
    } 
    if (!parsingHead) {
      val amountToTake = math.min(bodySize, buffer.bytesRemaining)
      write(buffer, amountToTake)
      bodySize -= amountToTake
      if (bodySize == 0) {
        complete[Unit](body)
        parsingHead = true
      }
    }
  }

}

trait HttpMessageEncoding[T <: HttpMessage] {
  def commonHeaders: Array[Header]
  def timeKeeper: TimeKeeper

  final def encode(message: T, buffer: WriteBuffer) {
    message.encodeFirstLine(buffer)
    buffer.write(ContentLengthPrefix)
    buffer.write(message.body.data.length)
    buffer.write(Newline)
    if (message.body.contentType.isDefined) {
      buffer.write(message.body.contentType.get.header.encodedLine(timeKeeper))
    }

    val h = message.headers.listIterator(0)
    while (h.hasNext) {
      buffer.write(h.next.encodedLine(timeKeeper))
    }

    var i = 0
    while (i < commonHeaders.length) {
      buffer.write(commonHeaders(i).encodedLine(timeKeeper))
      i += 1
    }
    buffer.write(Newline)
    buffer.write(message.body.data)
  }
}

class HttpMessageEncoder[T <: HttpMessage](
  val commonHeaders: Array[Header] = new Array(0), 
  val timeKeeper: TimeKeeper
) extends HttpMessageEncoding[T] {

  def encodeString(message: T): String = {
    val buf = new WriteBufferImpl(100, false)
    encode(message, buf)
    buf.data.readString
  }
}


class HttpServerCodec(
  onDecode: HttpRequest => Unit,
  val timeKeeper: TimeKeeper,
  val commonHeaders: Array[Header]
) 
extends Codec[HttpRequest, HttpResponse] with HttpMessageDecoder with FastArrayBuilding with HttpMessageEncoding[HttpResponse] {

  def finishDecode(firstLine: Array[Byte], headers: LinkedList[Header], body: Array[Byte]) {
    onDecode(new ParsedHttpRequest(firstLine, headers, Body(body, None)))
  }

}

class HttpClientCodec(
  onDecode: HttpResponse => Unit,
  val timeKeeper: TimeKeeper,
  val commonHeaders: Array[Header]
) 
extends Codec[HttpResponse, HttpRequest] with HttpMessageDecoder with FastArrayBuilding with HttpMessageEncoding[HttpRequest] {

  def finishDecode(firstLine: Array[Byte], headers: LinkedList[Header], body: Array[Byte]) {
    onDecode(new ParsedHttpResponse(firstLine, headers, Body(body, None)))
  }

}
