package scalene.http

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.{Arrays, Date, LinkedList, Locale, TimeZone}

import microactor.Pool
import scalene._
import scalene.util._

import HttpParsing._

class HttpVersion(strValue: String) {

  val stringValue = s"HTTP/${strValue}"
  val bytes: Array[Byte] = stringValue.getBytes
}
object HttpVersion {
  val `1.0` = new HttpVersion("1.0")
  val `1.1` = new HttpVersion("1.1")
  val `2.0` = new HttpVersion("2.0")
}


class Method(val name: String) {
  val bytes = name.getBytes   

  val lFirst = bytes(0)
  val uFirst = name.toUpperCase.getBytes()(0)

  def url(url: String): RouteBuilding2 = RouteBuilding2(this, url)
}

object Method {
  val Get   = new Method("GET")
  val Post  = new Method("POST")
}

case class ResponseCode(code: Int, codeMessage: String) {
  val v0FirstLine = s"HTTP/1.0 $code $codeMessage\r\n".getBytes
  val v1FirstLine = s"HTTP/1.1 $code $codeMessage\r\n".getBytes
}

object ResponseCode {
  val Ok = ResponseCode(200, "OK")
  val NotFound = ResponseCode(404, "NOT FOUND")
  val Error = ResponseCode(500, "ERROR")

  val codes = Map[Int, ResponseCode](
    200 -> Ok,
    404 -> NotFound,
    500 -> Error
  )

  def apply(intCode: Int): ResponseCode = codes(intCode)

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


trait HttpMessage {
  def headers: LinkedList[Header]
  def body: Body
  def version: HttpVersion

  def encodeFirstLine(buf: WriteBuffer): Unit
}






case class ContentType(value: String) {
  val header = Header("Content-Type", value)
}

object ContentType {
  val `text/plain` = ContentType("text/plain")
  val `application/json` = ContentType("application/json")
}

case class Body(data: Array[Byte], contentType: Option[ContentType]) {

  def ok        = HttpResponse(ResponseCode.Ok, this)
  def error     = HttpResponse(ResponseCode.Error, this)
  def notFound  = HttpResponse(ResponseCode.NotFound, this)

}

trait BodyFormatter[T] {
  def format(item: T): Array[Byte]
  def contentType: Option[ContentType]
}

object Body {

  implicit class BodyLifting[T](val value: T) extends AnyVal {
    def ok(implicit formatter: BodyFormatter[T]) = Body(formatter.format(value), formatter.contentType).ok
  }

  val Empty = Body(Nil.toArray, None)

  def plain(str: String) = Body(str.getBytes, Some(ContentType.`text/plain`))
  def json(encoded: String) = Body(encoded.getBytes, Some(ContentType.`application/json`))
}

object HttpClient {
  def futureClient(config: BasicClientConfig)(implicit pool: Pool) = {
    new FutureClient(
      env => client(config, env),
      config
    )
  }

  def client(config: BasicClientConfig, env: WorkEnv) = {
    new BasicClient[HttpRequest, HttpResponse](f => new HttpClientCodec(f, env.time, new Array(0)), config, env)
  }
}

  
