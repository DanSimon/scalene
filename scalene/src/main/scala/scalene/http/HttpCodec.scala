package scalene.http

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.{Arrays, Date, LinkedList, Locale, TimeZone}
import scalene._
import scalene.util._
import scalene.stream._


object HttpParsing {
  val SPACE_BYTE = ' '.toByte
  val CODE_START = HttpVersion.`1.1`.stringValue.length + 1

  
  val ContentLengthPrefix = "Content-Length: ".getBytes
  val Newline = "\r\n".getBytes

}
import HttpParsing._


trait HttpMessageDecoder extends LineParser {

  def initSize = 100
  val includeNewline = false

  def finishDecode(firstLine: Array[Byte], headers: Headers, body: BodyData)

  final val zeroFirstLine = new Array[Byte](0)

  private var buildFirstLine = new Array[Byte](0)
  private var buildHeaders = new LinkedList[Header]
  private var buildContentLength = 0
  private var buildTransferEncoding: Option[TransferEncoding] = None
  private var currentStreamManager: StreamManager = NoBodyManager

  val headers = new Headers(new LinkedList[Header])

  @inline
  final def buildMessage(): StreamManager = {
    val headers = new ParsedHeaders(
      headers = buildHeaders,
      transferEncodingOpt = buildTransferEncoding,
      contentType = None,
      contentLength = if (buildContentLength == 0) None else Some(buildContentLength),
      connection = None
    )
    val streamManager = buildTransferEncoding match {
      case None => if (buildContentLength == 0) {
        NoBodyManager
      } else {
        new BasicStreamManager(buildContentLength)
      }
      case Some(TransferEncoding.Chunked) => {
        ???
      }
    }
    val builder = StreamBuilder(streamManager)
    finishDecode(buildFirstLine,  headers, BodyData.Stream(builder))
    buildHeaders = new LinkedList[Header]
    buildFirstLine = zeroFirstLine
    buildContentLength = 0
    buildTransferEncoding = None
    streamManager
  }

  //returns true if we've finished reading the header
  @inline
  final def onComplete(buf: ReadBuffer): Boolean = {
    if (buf.size == 0) { //0 size is the blank newline at the end of the head
      currentStreamManager = buildMessage()
      true
    } else {
      if (buildFirstLine.length == 0) {
        buildFirstLine = buf.readAll
      } else {          
        val header = buf.readAll
        parseSpecialHeader(header)
        buildHeaders.add(new StaticHeader(header))
      }
      false
    }
  }

  @inline
  final protected def parseSpecialHeader(header: Array[Byte]): Unit = {
    if (buildContentLength == 0 && ParsingUtils.caseInsensitiveSubstringMatch(header, Headers.ContentLength.bytes)) {
      buildContentLength = trimStringToInt(header, Headers.ContentLength.bytes.length + 2)
    } else if (ParsingUtils.caseInsensitiveSubstringMatch(header, Headers.TransferEncoding.bytes)) {
      buildTransferEncoding = Some(TransferEncoding.fromHeaderLine(header))
    }
  }

  @inline
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

  final def decode(buffer: ReadBuffer): Unit = {
    while (buffer.hasNext) {
      while (parsingHead && buffer.hasNext) {
        parsingHead = !parse(buffer)
      } 
      if (!parsingHead) {
        if (currentStreamManager.isDone) {
          parsingHead = true
        } else {
          currentStreamManager.push(buffer)
        }
      }
    }
  }

}

trait HttpMessageEncoding[T <: HttpMessage] {
  def commonHeaders: Array[Header]
  def timeKeeper: TimeKeeper

  final def encode(message: T, buffer: WriteBuffer):Option[Stream[Writable]] =  {
    message.encodeFirstLine(buffer)
    if (message.body.contentType.isDefined) {
      buffer.write(message.body.contentType.get.header.encodedLine(timeKeeper))
    }
    message.headers.encode(buffer, timeKeeper)

    var i = 0
    while (i < commonHeaders.length) {
      buffer.write(commonHeaders(i).encodedLine(timeKeeper))
      i += 1
    }
    message.body.data match {
      case BodyData.Static(buf) => {
        buffer.write(ContentLengthPrefix)
        buffer.write(buf.length)
        buffer.write(Newline)
        buffer.write(Newline)
        buffer.write(buf)
        None
      }
      case BodyData.Stream(stream) => {
        buffer.write(Headers.TransferEncoding.Chunked.encodedLine(timeKeeper))
        buffer.write(Newline)
        Some(stream.map[ChunkEncoder](ChunkEncoder(_)).chain(new HttpChunkEncodingSink).map{x => x:Writable})
      }
    }
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
extends Codec[HttpRequest, HttpResponse] with HttpMessageDecoder  with HttpMessageEncoding[HttpResponse] {

  final def finishDecode(firstLine: Array[Byte], headers: Headers, body: BodyData) {
    onDecode(new ParsedHttpRequest(firstLine, headers, Body(body, None)))
  }

}

class HttpClientCodec(
  onDecode: HttpResponse => Unit,
  val timeKeeper: TimeKeeper,
  val commonHeaders: Array[Header]
) 
extends Codec[HttpResponse, HttpRequest] with HttpMessageDecoder  with HttpMessageEncoding[HttpRequest] {

  final def finishDecode(firstLine: Array[Byte], headers: Headers, body: BodyData) {
    onDecode(new ParsedHttpResponse(firstLine, headers, Body(body, None)))
  }

}

trait StreamManager extends Sink[ReadBuffer] {

  def isDone: Boolean
}

class BasicStreamManager(bodySize: Int) extends LiveSink[ReadBuffer] with StreamManager {

  private var bodyRemaining = bodySize

  def isDone = bodyRemaining == 0

  final override def push(buffer: ReadBuffer): PushResult = {
    val prevBytes = buffer.bytesRemaining
    val prevBodyRemaining = bodyRemaining
    val pushResult = if (bodyRemaining >= buffer.bytesRemaining) {
      bodyRemaining -= buffer.bytesRemaining
      super.push(buffer)
    } else {
      val prevLimit = buffer.buffer.limit()
      buffer.buffer.limit(bodyRemaining)
      bodyRemaining = 0
      val res = super.push(buffer)
      buffer.buffer.limit(prevLimit)
      res
    }
    if (buffer.bytesRemaining == prevBytes) {
      //use it or lose it!
      println(s"skipping ${prevBodyRemaining - bodyRemaining} buffer bytes")
      buffer.skip(prevBodyRemaining - bodyRemaining)
    }
    pushResult
  }

}

object NoBodyManager extends BasicStreamManager(0)

//This collects raw data into an array.  Should not contain chunk headers
class BodyCollector extends Collector[ReadBuffer, ReadBuffer] with FastArrayBuilding[Unit] {

  def initSize = 100
  def shrinkOnComplete = true

  def onComplete(buf: ReadBuffer): Unit = {
    result.succeed(buf)
  }

  val result = new PromiseAsync[ReadBuffer]

  def push(buffer: ReadBuffer): PushResult = {
    write(buffer)
    PushResult.Ok
  }

  override def onClose(): Unit = {
    complete()
  }

  override def onError(reason: Throwable) = result.fail(reason)

}

case class ChunkEncoder(chunkData: ReadBuffer) extends Writable {

  def writeTo(buffer: WriteBuffer): Unit = {
    buffer.write(chunkData.bytesRemaining.toHexString.getBytes)
    buffer.write(Newline)
    chunkData.writeTo(buffer)
    buffer.write(Newline)
  }

}

class HttpChunkEncodingSink extends LiveSink[ChunkEncoder] {

  override def push(item: ChunkEncoder): PushResult = super.push(item)

  override def close(): Unit = {
    super.push(HttpChunkEncodingSink.EndChunk)
    super.close()
  }

  override def error(reason: Throwable) = super.error(reason)

}

object HttpChunkEncodingSink {

  val EndChunk = ChunkEncoder(ReadBuffer(new Array[Byte](0)))
}


