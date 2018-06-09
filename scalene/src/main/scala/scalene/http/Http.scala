package scalene

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.{Arrays, Date, LinkedList, Locale, TimeZone}

import microactor.Pool
import util._

object HttpParsing {
  val SPACE_BYTE = ' '.toByte

  
  val ContentLengthPrefix = "Content-Length: ".getBytes
  val Newline = "\r\n".getBytes

}
import HttpParsing._


class Method(val name: String) {
  val bytes = name.getBytes   

  val lFirst = bytes(0)
  val uFirst = name.toUpperCase.getBytes()(0)

  def url(url: String): RouteBuilding2 = RouteBuilding2(this, url)
}

object Method {
  val Get   = new Method("get")
  val Post  = new Method("post")
}

case class ResponseCode(code: Int, codeMessage: String) {
  val v0FirstLine = s"HTTP/1.0 $code $codeMessage\r\n".getBytes
  val v1FirstLine = s"HTTP/1.1 $code $codeMessage\r\n".getBytes
}

object ResponseCode {
  val Ok = ResponseCode(200, "OK")
  val NotFound = ResponseCode(404, "NOT FOUND")
  val Error = ResponseCode(500, "ERROR")
}

trait Header {
  def key: String
  def value: String

  def encodedLine(time: TimeKeeper): Array[Byte]

  override def equals(that: Any) = that match {
    case e: Header => key == e.key && value == e.value
    case _ => false
  }
}

class StaticHeader(val encodedLine: Array[Byte]) extends Header {

  def encodedLine(time: TimeKeeper) : Array[Byte] = encodedLine

  def this(key: String, value: String) = this(s"$key: $value\r\n".getBytes)

  private lazy val valueStart = encodedLine.indexOf(':'.toByte) + 1
  lazy val key                = new String(encodedLine, 0, valueStart - 1).toLowerCase
  lazy val value              = new String(encodedLine, valueStart, encodedLine.length - valueStart).trim

}

class DateHeader(initialTime: Long = System.currentTimeMillis) extends Header{

  private val sdf = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.ENGLISH)
  sdf.setTimeZone(TimeZone.getTimeZone("GMT"))

  val key = "Date"
  val value = sdf.format(new Date(lastUpdated))

  private var lastUpdated = initialTime
  private var lastDateString = createLine()

  private def createLine() = s"Date: ${sdf.format(new Date(lastUpdated))}\r\n".getBytes

  def encodedLine(time: TimeKeeper): Array[Byte] = {
    if (time() - lastUpdated >= 1000) {
      lastDateString = createLine
      lastUpdated = time()
    }
    lastDateString
  }


}

object Header {
  def apply(key: String, value: String) : Header = new StaticHeader(key, value)
}

case class BasicHttpRequest(firstLine: Array[Byte], headers: LinkedList[Header], body: Array[Byte]) {
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

  def fastMethodUrl(method: Method, url: Array[Byte]): Boolean = {
    if (firstLine(0) == method.lFirst || firstLine(0) == method.uFirst) {
      val methodLength = method.bytes.length
      val urlLength = url.length
      val urlStart = methodLength + 1
      //url.length <= firstLine.length - method.bytes.length &&
      firstLine(urlStart + urlLength) == SPACE_BYTE &&
      Arrays.mismatch(firstLine, urlStart, urlLength + urlStart, url, 0, urlLength) == -1
    } else false
  }


  def methodEquals(method: Method): Boolean = ParsingUtils.caseInsensitiveSubstringMatch(firstLine, method.bytes)
}


case class BasicHttpResponse(code: ResponseCode, headers: Array[Header], body: Body)
object BasicHttpResponse {
  private val emptyHeaders = new Array[Header](0)
  def apply(code: ResponseCode, body:Body): BasicHttpResponse = BasicHttpResponse(code, emptyHeaders, body)
}

class BasicHttpCodec(
  onDecode: BasicHttpRequest => Unit,
  timeKeeper: TimeKeeper,
  commonHeaders: Array[Header]
) 
extends Codec[BasicHttpRequest, BasicHttpResponse](onDecode) with FastArrayBuilding {

  final val zeroFirstLine = new Array[Byte](0)

  var buildFirstLine = new Array[Byte](0)
  var buildHeaders = new LinkedList[Header]
  var buildContentLength = 0

  final val contentLengthKey = "content-length".getBytes

  final def body(buf: ReadBuffer): Unit = {
    onDecode(BasicHttpRequest(buildFirstLine,  buildHeaders, buf.takeAll))
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
        buildFirstLine = buf.takeAll
      } else {          
        val header = buf.takeAll
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

  def endOfStream() {}

  //parsing stuff
  private var parsingHead = true
  private var bodySize = 0

  final val headParser = new LineParser(line, false, 100)
  final val zeroBody = ReadBuffer(ByteBuffer.wrap(new Array[Byte](0)))

  def initSize = 1024
  def shrinkOnComplete = true

  final def decode(buffer: ReadBuffer): Unit = {
    while (parsingHead && buffer.hasUnreadData) {
      bodySize = headParser.parse(buffer)
      if (bodySize == 0) {
        body(zeroBody)        
      } else if (bodySize > 0) {
        parsingHead = false        
      }
    } 
    if (!parsingHead) {
      val amountToTake = math.min(bodySize, buffer.remaining)
      write(buffer, amountToTake)
      bodySize -= amountToTake
      if (bodySize == 0) {
        complete[Unit](body)
        parsingHead = true
      }
    }
  }

}


class BasicRoute(val method: Method, val fullUrl: String, val handler: BasicHttpRequest => Async[BasicHttpResponse]) {
  private val flMatch = s"${method.name.toUpperCase} $fullUrl HTTP/1.1".getBytes

  def isMatch(req: BasicHttpRequest) = Arrays.equals(req.firstLine, flMatch)

}

class BasicRouter(routeSeq: Seq[BasicRoute]) extends RequestHandler[BasicHttpRequest, BasicHttpResponse] {
  var _context: Option[RequestHandlerContext] = None

  private val routes = routeSeq.toArray
  private val NoRouteResponse = Async.successful(BasicHttpResponse(ResponseCode.NotFound, Body.plain("unknown path")))

  def handleRequest(input: BasicHttpRequest) = {
    var i = 0
    while (i < routes.length && !routes(i).isMatch(input)) { i += 1 }
    if (i < routes.length) {
      routes(i).handler(input)
    } else {
      NoRouteResponse
    }
  }

  def handleError(req: Option[BasicHttpRequest], reason: Throwable) = BasicHttpResponse(
    ResponseCode.Error,
    Body.plain(reason.getMessage)
  )

  override def onInitialize(ctx: RequestHandlerContext): Unit = {
    _context = Some(ctx)
  }

}

case class ContentType(value: String) {
  val header = Header("Content-Type", value)
}

object ContentType {
  val `text/plain` = ContentType("text/plain")
  val `application/json` = ContentType("application/json")
}

case class Body(data: Array[Byte], contentType: Option[ContentType]) {

  def ok        = BasicHttpResponse(ResponseCode.Ok, this)
  def error     = BasicHttpResponse(ResponseCode.Error, this)
  def notFound  = BasicHttpResponse(ResponseCode.NotFound, this)

}

object Body {
  val Empty = Body(Nil.toArray, None)

  def plain(str: String) = Body(str.getBytes, Some(ContentType.`text/plain`))
}

case class HttpServerSettings(
  serverName: String,
  server: ServerSettings,
  commonHeaders: Seq[Header] = List(new DateHeader)
)

object HttpServer {
  def start(settings: HttpServerSettings, routes: Seq[BasicRoute]): Server.Server = {
    implicit val pool = new Pool
    val commonHeaders = (settings.commonHeaders :+ Header("Server", settings.serverName)).toArray
    val factory: ConnectionContext => ServerConnectionHandler = ctx => {
      new ServiceServer((x: BasicHttpRequest => Unit) => new BasicHttpCodec(x, ctx.time, commonHeaders), new BasicRouter(routes))
    }
    Server.start(settings.server, factory, new RefreshOnDemandTimeKeeper(new RealTimeKeeper))
  }
}

case class RouteBuilding2(method: Method, url: String) {
  def to(handler: BasicHttpRequest => Async[BasicHttpResponse]) : BasicRoute = {
    new BasicRoute(method, url, handler)
  }

  def to(syncResponse: => BasicHttpResponse): BasicRoute = to(_ => Async.successful(syncResponse))
}

