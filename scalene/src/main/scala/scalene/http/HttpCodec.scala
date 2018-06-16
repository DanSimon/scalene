package scalene.http

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.{Arrays, Date, LinkedList, Locale, TimeZone}
import scalene._
import scalene.util._


object HttpParsing {
  val SPACE_BYTE = ' '.toByte

  
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
    //println("line")
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
      println(s"got content length $buildContentLength")
    }
  }


  final private def trimStringToInt(line: Array[Byte], paddedStartIndex: Int): Int = {
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

class HttpServerCodec(
  onDecode: BasicHttpRequest => Unit,
  timeKeeper: TimeKeeper,
  commonHeaders: Array[Header]
) 
extends Codec[BasicHttpRequest, BasicHttpResponse] with HttpMessageDecoder with FastArrayBuilding {

  def finishDecode(firstLine: Array[Byte], headers: LinkedList[Header], body: Array[Byte]) {
    onDecode(BasicHttpRequest(firstLine, headers, body))
  }

  def encode(message: BasicHttpResponse, buffer: WriteBuffer) {
    buffer.write(message.code.v1FirstLine)
    buffer.write(ContentLengthPrefix)
    buffer.write(message.body.data.length)
    buffer.write(Newline)
    if (message.body.contentType.isDefined) {
      buffer.write(message.body.contentType.get.header.encodedLine(timeKeeper))
    }
    var i = 0
    while (i < message.headers.length) {
      buffer.write(message.headers(i).encodedLine(timeKeeper))
      i += 1
    }
    i = 0
    while (i < commonHeaders.length) {
      buffer.write(commonHeaders(i).encodedLine(timeKeeper))
      i += 1
    }
    buffer.write(Newline)
    buffer.write(message.body.data)
  }


}
