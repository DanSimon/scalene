//Portions of this derived from
//https://github.com/tumblr/colossus/blob/master/colossus/src/main/scala/colossus/protocols/http/Header.scala
//Copyright (c) 2017 Tumblr

package scalene.http
import scalene._
import scalene.util._

class ParseException(message: String) extends Exception(message)

class ParsedHeaders(
    data: Array[Byte],
    lineStarts: Array[Int],
    override val transferEncoding: TransferEncoding,
    override val contentLength: Option[Int]
    //override val connection: Option[Connection]
) extends Headers {

  def size = lineStarts.length

  def toSeq = {
    var i = 0
    val headers = new Array[Header](lineStarts.length)
    while (i < lineStarts.length) {
      val end = if (i + 1 < lineStarts.length) lineStarts(i + 1) else data.length
      val line = new String(data, lineStarts(i), end - lineStarts(i) - 2)
      val split = line.split(":")
      headers(i) = Header(split(0), split(1))
      i += 1
    }
    headers
  }



  def encode(buffer: WriteBuffer, timeKeeper: TimeKeeper) : Unit = {}

}

case class ArrayHeaders(headers: Array[Header]) extends Headers {


  def size = headers.length

  def toSeq = headers.toSeq

  def encode(buffer: WriteBuffer, timeKeeper: TimeKeeper) : Unit = {
    var i = 0
    while (i < headers.length) {
      buffer.write(headers(i).encodedLine(timeKeeper))
    }
  }
}

trait Headers {

  def firstValue(name: String): Option[String] = toSeq.find{_.key == name}.map{_.value}

  def allValues(name: String): Seq[String] = toSeq.filter{_.key == name}.map{_.value}

  def contentLength: Option[Int] = firstValue(Headers.ContentLength.name).map { _.toInt }

  def transferEncoding: TransferEncoding =
    firstValue(Headers.TransferEncoding.name).map(TransferEncoding(_)).getOrElse(TransferEncoding.Identity)

  def connection: Option[Connection] = firstValue(Headers.Connection.name).map(Connection(_))

  def contentType: Option[String] = firstValue(Headers.ContentType.name)


  def size: Int
  def toSeq: Seq[Header]

  def encode(buffer: WriteBuffer, timeKeeper: TimeKeeper) : Unit

  override def equals(that: Any): Boolean = that match {
    case that: Headers => this.toSeq.toSet == that.toSeq.toSet
    case _                 => false
  }

  override def hashCode(): Int = this.toSeq.toSet.hashCode()

  override def toString = "[" + toSeq.map { _.toString }.mkString(" ") + "]"

}

class HeaderKey(str: String) {
  val name = str.toLowerCase
  val bytes = name.getBytes
}
object HeaderKey {
  def apply(str: String) : HeaderKey = new HeaderKey(str)
}

object Headers {

  val Accept           = HeaderKey("accept")
  val AcceptEncoding   = HeaderKey("accept-encoding")
  val Connection       = HeaderKey("connection")
  val ContentLength    = HeaderKey("content-length")
  val ContentType      = HeaderKey("content-type")
  val ContentEncoding  = HeaderKey("content-encoding")
  val CookieHeader     = HeaderKey("cookie")
  val Host             = HeaderKey("host")
  val SetCookie        = HeaderKey("set-cookie")
  val TransferEncoding = new HeaderKey("transfer-encoding") {
    val Chunked = new StaticHeader(name, "Chunked")
  }

  def apply(hdrs: Header*): Headers = Headers.fromSeq(hdrs)

  def fromString(hdrs: (String, String)*): Headers =
    Headers.fromSeq(hdrs.map { case (k, v) => Header(k, v) })

  def fromSeq(seq: Seq[Header]): Headers = ArrayHeaders(seq.toArray)
  
  val Empty = ArrayHeaders(new Array(0))
}

sealed trait TransferEncoding {
  def value: String

  lazy val header: Header = Header("Transfer-Encoding", value)
}

object TransferEncoding {
  case object Identity extends TransferEncoding {
    val value = "identity"
  }

  case object Chunked extends TransferEncoding {
    val value = "chunked"
  }

  private val all = Seq(Identity, Chunked)
  def apply(str: String): TransferEncoding = {
    val toFind = str.toLowerCase
    all.find(_.value == toFind).getOrElse(throw new ParseException(s"Invalid transfer-encoding header value '$str'"))
  }
  def fromHeaderLine(line: Array[Byte]): TransferEncoding = {
    var b = Headers.TransferEncoding.bytes.length + 1
    while (line(b) == HttpParsing.SPACE_BYTE) { b += 1 }
    if (line(b) == 'c' || line(b) == 'C') {
      TransferEncoding.Chunked
    } else {
      TransferEncoding.Identity
    }
  }
}

sealed trait ContentEncoding {
  def value: String
}
object ContentEncoding {

  case object Identity extends ContentEncoding {
    val value = "identity"
  }

  case object Gzip extends ContentEncoding {
    val value = "gzip"
  }

  case object Deflate extends ContentEncoding {
    val value = "deflate"
  }

  case object Compressed extends ContentEncoding {
    val value = "compressed"
  }

  private val all = Seq(Gzip, Deflate, Compressed, Identity)
  def apply(str: String): ContentEncoding = {
    val toFind = str.toLowerCase
    all.find(_.value == toFind).getOrElse(throw new ParseException("Invalid content-encoding header value '$str'"))
  }
}

sealed trait Connection {
  def value: String

  lazy val header: Header = Header("Connection", value)
}
object Connection {
  case object KeepAlive extends Connection {
    val value = "keep-alive"
  }
  case object Close extends Connection {
    val value = "close"
  }
  case object Upgrade extends Connection {
    val value = "upgrade"
  }

  private val all = Seq(Close, KeepAlive, Upgrade)
  def apply(str: String): Connection = {
    val toFind = str.toLowerCase
    all.find(_.value == toFind).getOrElse(throw new ParseException(s"Invalid connection header value '$str'"))
  }
}

