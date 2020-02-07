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

  
  val ContentLengthPrefix: Array[Byte] = "Content-Length: ".getBytes
  val Newline: Array[Byte] = "\r\n".getBytes
  val DoubleNewline: Array[Byte] = Newline ++ Newline

}
import HttpParsing._


trait HttpMessageDecoder extends LineParser {

  def initSize = 100
  val includeNewline = false

  def finishDecode(firstLine: BoundedArray, headers: Headers, body: BodyData)

  final val zeroFirstLine = BoundedArray(new Array[Byte](0))
  private val NoBodyStream = BodyData.Stream(StreamBuilder(NoBodyManager))

  private var messages = 0L
  def messagesDecoded:Long = messages

  var currentSize = 0L
  def currentMessageSize: Long = currentSize

  private var buildFirstLine = zeroFirstLine
  private var buildHeaders = new LinkedList[Header]
  private var buildContentLength: Option[Int] = None
  private var buildTransferEncoding: Option[TransferEncoding] = None
  private var currentStreamManager: StreamManager = NoBodyManager

  val headers = new Headers(new LinkedList[Header])

  @inline
  final def buildMessage(): Unit = {
    val headers = new ParsedHeaders(
      headers = buildHeaders,
      transferEncodingOpt = buildTransferEncoding,
      contentType = None,
      contentLength = buildContentLength,
      connection = None
    )
    val body = buildTransferEncoding match {
      case None | Some(TransferEncoding.Identity) => if (buildContentLength.isEmpty) {
        NoBodyStream
      } else {
        currentStreamManager = new BasicStreamManager(buildContentLength.get)
        BodyData.Stream(StreamBuilder(currentStreamManager))
      }
      case Some(TransferEncoding.Chunked) => {
        currentStreamManager = new ChunkedStreamManager
        BodyData.Stream(StreamBuilder(currentStreamManager))
      }
    }

    finishDecode(buildFirstLine,  headers, body)
    //if the body stream was unused we have to complete it ourselves so the data is still consumed
    currentStreamManager.setBlackHoleIfUnset()
    currentStreamManager = NoBodyManager
    buildHeaders = new LinkedList[Header]
    buildFirstLine = zeroFirstLine
    buildContentLength = None
    buildTransferEncoding = None
  }

  //returns true if we've finished reading the header
  @inline
  final def onComplete(arr: BoundedArray): Boolean = {
    if (arr.length == 0) { //0 size is the blank newline at the end of the head
      buildMessage()
      true
    } else {
      if (buildFirstLine.length == 0) {
        buildFirstLine = arr
      } else {          
        val header = arr
        parseSpecialHeader(header)
        buildHeaders.add(new StaticHeader(header))
      }
      false
    }
  }

  @inline
  final protected def parseSpecialHeader(header: BoundedArray): Unit = {
    if (buildContentLength.isEmpty && ParsingUtils.caseInsensitiveSubstringMatch(header, Headers.ContentLength.bytes)) {
      buildContentLength = Some(trimStringToInt(header, Headers.ContentLength.bytes.length + 2))
    } else if (ParsingUtils.caseInsensitiveSubstringMatch(header, Headers.TransferEncoding.bytes)) {
      buildTransferEncoding = Some(TransferEncoding.fromHeaderLine(header.trimmedCopy))
    }
  }

  @inline
  final private def trimStringToInt(line: BoundedArray, paddedStartIndex: Int): Int = {
    var i = line.length - 1
    var build = 0
    var mult = 1
    val start = paddedStartIndex + line.start
    while (i >= start) {
      val c = line.raw(i)
      if (c != ' '.toByte) {
        build += (c - 48) * mult
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
      currentSize += buffer.bytesRead
      if (!parsingHead) {
        if (!currentStreamManager.isDone) {
          currentStreamManager.push(buffer)
          //FIXME: this is wrong
          currentSize += buffer.bytesRead
        }
        //we have to immediately check again since it could be the end of the
        //buffer and the loop will exit
        if (currentStreamManager.isDone) {
          parsingHead = true
          messages += 1
          currentSize = 0
          currentStreamManager.close()
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
        buffer.write(DoubleNewline)
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

  final def finishDecode(firstLine: BoundedArray, headers: Headers, body: BodyData) {
    onDecode(new ParsedHttpRequest(firstLine, headers, Body(body, None)))
  }

}

class HttpClientCodec(
  onDecode: HttpResponse => Unit,
  val timeKeeper: TimeKeeper,
  val commonHeaders: Array[Header]
) 
extends Codec[HttpResponse, HttpRequest] with HttpMessageDecoder  with HttpMessageEncoding[HttpRequest] {

  final def finishDecode(firstLine: BoundedArray, headers: Headers, body: BodyData) {
    onDecode(new ParsedHttpResponse(firstLine.trimmedCopy, headers, Body(body, None)))
  }

}

trait StreamManager extends LiveSink[ReadBuffer] {

  def isDone: Boolean
}

trait SubBufferSink extends Sink[ReadBuffer] {

  private var bodyRemaining = 0

  protected def bytesRemaining = bodyRemaining
  protected def updateRemaining(newSize: Int): Unit = {
    bodyRemaining = newSize
  }

  abstract override def push(buffer: ReadBuffer): PushResult = {
    val prevBytes = buffer.bytesRemaining
    val prevBodyRemaining = bodyRemaining
    val pushResult = if (bodyRemaining >= buffer.bytesRemaining) {
      bodyRemaining -= buffer.bytesRemaining
      super.push(buffer)
    } else {
      val prevLimit = buffer.buffer.limit()
      buffer.buffer.limit(buffer.buffer.position() + bodyRemaining)
      bodyRemaining = 0
      val res = super.push(buffer)
      buffer.buffer.limit(prevLimit)
      res
    }
    if (buffer.bytesRemaining == prevBytes) {
      //use it or lose it!
      buffer.skip(prevBodyRemaining - bodyRemaining)
    }
    pushResult
  }

}

class BasicStreamManager(bodySize: Int) extends LiveSink[ReadBuffer] with SubBufferSink with StreamManager {

  def isDone = bytesRemaining == 0

  updateRemaining(bodySize)

}

class ChunkedStreamManager extends LiveSink[ReadBuffer] with SubBufferSink with StreamManager with LineParser {
  //parsing stuff
  sealed trait State
  case object ParsingHead extends  State
  case object ParsingTail extends State
  case object ParsingBody extends State
  case object Done extends State
  private var state: State = ParsingHead

  def includeNewline = false
  def initSize = 10

  private var _isDone = false
  def isDone = _isDone

  def onComplete(line: BoundedArray): Boolean = {
    if (state == ParsingHead) {
      val size = Integer.parseInt(line.toString, 16)
      if (size == 0) {
        _isDone = true
      } 
      updateRemaining(size)      
    }
    true
  }

  final override def push(buffer: ReadBuffer): PushResult = {
    //the buffer may contain multiple chunks, so we have to consume them all
    var lastResult: PushResult = PushResult.Ok
    while (buffer.hasNext && state != Done) {
      state match {
        case ParsingHead => {
          val doneParsingHead = parse(buffer)
          if (doneParsingHead) {
            state = ParsingBody
          }
        }
        case ParsingBody => {
          lastResult = super.push(buffer)
          if (bytesRemaining == 0) {
            state = ParsingTail
          }
        }
        case ParsingTail => {
          val doneParsingTail = parse(buffer)
          if (doneParsingTail) {
            state = ParsingHead
          }
        }
        case Done => {}
      }
    }
    lastResult
  }

}

object NoBodyManager extends BasicStreamManager(0) {
  setBlackHoleIfUnset()
}

//This collects raw data into an array.  Should not contain chunk headers
class BodyCollector extends Collector[ReadBuffer, ReadBuffer] with FastArrayBuilding {

  def initSize = 100
  def shrinkOnComplete = true

  def onComplete(arr: BoundedArray): Boolean = {
    result.succeed(ReadBuffer(arr.trimmedCopy))
    false
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


