//Portions of this derived from
//https://github.com/tumblr/colossus/blob/master/colossus/src/main/scala/colossus/protocols/http/Header.scala
//Copyright (c) 2017 Tumblr

import java.util.{List => JList}

package scalene

class ParsedHeaders(
    headers: JList[Header],
    val transferEncodingOpt: Option[TransferEncoding],
    override val contentType: Option[String],
    override val contentLength: Option[Int],
    override val connection: Option[Connection]
) extends Headers(headers) {

  def this(
      headers: Headers,
      transferEncodingOpt: Option[TransferEncoding],
      contentType: Option[String],
      contentLength: Option[Int],
      connection: Option[Connection]
  ) = this(headers.headers, transferEncodingOpt, contentType, contentLength, connection)

  override def transferEncoding =
    if (transferEncodingOpt.isDefined) transferEncodingOpt.get else TransferEncoding.Identity

  override def encode(buffer: DataOutBuffer) {
    transferEncodingOpt.foreach { _.header.encode(buffer) }
    connection.foreach { _.header.encode(buffer) }
    contentLength.foreach { c =>
      Header.encodeContentLength(buffer, c)
    }
    super.encode(buffer)
  }

}

class Headers(private[http] val headers: JList[Header]) {

  def firstValue(name: String): Option[String] = {
    val l = name.toLowerCase
    toSeq.collectFirst { case x if (x.key == l) => x.value }
  }

  def allValues(name: String): Seq[String] = {
    val l = name.toLowerCase
    toSeq.collect { case x if (x.key == l) => x.value }
  }

  def contentLength: Option[Int] = firstValue(Headers.ContentLength).map { _.toInt }

  def transferEncoding: TransferEncoding =
    firstValue(Headers.TransferEncoding).map(TransferEncoding(_)).getOrElse(TransferEncoding.Identity)

  def connection: Option[Connection] = firstValue(Headers.Connection).map(Connection(_))

  def contentType: Option[String] = firstValue(Headers.ContentType)

  def +(kv: (String, String)): Headers = {
    val n = Header(kv._1, kv._2)
    this + n
  }

  def +(header: Header): Headers = {
    Headers.fromSeq(toSeq :+ header)
  }

  def size = headers.size

  def toSeq: Seq[Header] = headers.toArray(Array[Header]())

  def encode(buffer: WriteBuffer, timeKeeper: TimeKeeper) {
    val it = headers.iterator
    while (it.hasNext) {
      buffer.write(it.next.encodedLine(timeKeeper))
    }
  }

  def unsafeAppend(header: Header) {
    headers.add(header)
  }

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
  val TransferEncoding = HeaderKey("transfer-encoding")

  def apply(hdrs: Header*): Headers = Headers.fromSeq(hdrs)
  def fromString(hdrs: (String, String)*): Headers =
    Headers.fromSeq(hdrs.map { case (k, v) => Header(k, v) })

  def fromSeq(seq: Seq[Header]): Headers = {
    val l = new LinkedList[Header]
    seq.foreach(l.add)
    new Headers(l)
  }

  val Empty = new Headers(new LinkedList)
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
    while (line(b) == SPACE_BYTE) { b += 1 }
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

