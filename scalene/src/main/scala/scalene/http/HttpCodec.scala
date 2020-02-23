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


trait HttpMessageDecoder {

  def finishDecode(head: ParsedHead, body: BodyData)

  final val zeroFirstLine = new Array[Byte](0)
  private val NoBodyStream = BodyData.Stream(StreamBuilder(NoBodyManager))

  private var messages = 0L
  def messagesDecoded:Long = messages

  var currentSize = 0L
  def currentMessageSize: Long = currentSize

  private var buildFirstLine = new Array[Byte](0)
  private var buildContentLength: Option[Int] = None
  private var buildTransferEncoding: Option[TransferEncoding] = None
  private var currentStreamManager: StreamManager = NoBodyManager

  @inline
  final protected def parseSpecialHeader(header: Array[Byte]): Unit = {
    val b = header(0)
    if (b == 'c' || b == 'C' || b == 't' || b == 'T') {
      if (buildContentLength.isEmpty && ParsingUtils.caseInsensitiveSubstringMatch(header, Headers.ContentLength.bytes)) {
        buildContentLength = Some(trimStringToInt(header, Headers.ContentLength.bytes.length + 2))
      } else if (ParsingUtils.caseInsensitiveSubstringMatch(header, Headers.TransferEncoding.bytes)) {
        buildTransferEncoding = Some(TransferEncoding.fromHeaderLine(header))
      }
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
  private var rnCount = 0
  private var dataBuild: Array[Byte] = new Array(300)
  private var dataBuildPos: Int = 0
  private var dataLineStarts: Array[Int] = new Array(10)
  private var dataLineStartsPos: Int = 0

  //returns true if we done
  def parseHead(buffer: ReadBuffer): Boolean = {
    while (buffer.buffer.hasRemaining) {
      val b = buffer.buffer.get
      dataBuild(dataBuildPos) = b
      dataBuildPos += 1
      if (b == '\n'.toByte || b == '\r'.toByte) {
        rnCount += 1
        if (rnCount == 4) {
          rnCount = 0
          val data = Arrays.copyOf(dataBuild, dataBuildPos)
          dataBuildPos = 0
          val lineStarts = Arrays.copyOf(dataLineStarts, dataLineStartsPos)
          dataLineStartsPos = 0
          val head = ParsedHead(
            data,
            lineStarts(0) - 2,
            new ParsedHeaders(data, dataLineStarts, None, None, None, None)
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

          finishDecode(head, body)
          //if the body stream was unused we have to complete it ourselves so the data is still consumed
          currentStreamManager.setBlackHoleIfUnset()
          if (body != NoBodyStream)
            return true

        } else if (rnCount == 2) {
          dataLineStarts(dataLineStartsPos) = dataBuildPos
          dataLineStartsPos += 1
        }
      } else {
        rnCount = 0
      }
    }
    false
  }

  final def decode(buffer: ReadBuffer): Unit = {
    while (buffer.hasNext) {
      currentSize += buffer.bytesRead
      if (parseHead(buffer)) {
        if (!currentStreamManager.isDone) {
          currentStreamManager.push(buffer)
          //FIXME: this is wrong
          currentSize += buffer.bytesRead
        }
        //we have to immediately check again since it could be the end of the
        //buffer and the loop will exit
        if (currentStreamManager.isDone) {
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

  final def finishDecode(head: ParsedHead, body: BodyData) {
    onDecode(new ParsedHttpRequest(head, Body(body, None)))
  }

}

class HttpClientCodec(
  onDecode: HttpResponse => Unit,
  val timeKeeper: TimeKeeper,
  val commonHeaders: Array[Header]
) 
extends Codec[HttpResponse, HttpRequest] with HttpMessageDecoder  with HttpMessageEncoding[HttpRequest] {

  final def finishDecode(head: ParsedHead, body: BodyData) {
    //onDecode(new ParsedHttpResponse(firstLine, headers, Body(body, None)))
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

  def onComplete(line: Array[Byte]): Boolean = {
    if (state == ParsingHead) {
      val size = Integer.parseInt(new String(line), 16)
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
class BodyCollector extends Collector[ReadBuffer, ReadBuffer] with FastArrayBuilding[Unit] {

  def initSize = 100
  def shrinkOnComplete = true

  def onComplete(arr: Array[Byte]): Unit = {
    result.succeed(ReadBuffer(arr))
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


