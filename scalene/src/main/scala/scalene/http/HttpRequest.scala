package scalene
package http

import java.util.{Arrays, LinkedList}

import HttpParsing._

trait HttpRequest extends HttpMessage {
  def method: Method
  def url: String

  lazy val (path, query) = {
    val pieces = url.split("\\?", 2)
    (pieces(0), if (pieces.size > 1) Some(pieces(1)) else None)
  }

  lazy val parameters = new QueryParameters(query.getOrElse(""))

  def urlBytes: Array[Byte]

  def firstLine: Array[Byte]

  def encodeFirstLine(buffer: WriteBuffer) : Unit = {
    buffer.write(method.bytes)
    buffer.write(SPACE_BYTE)
    buffer.write(urlBytes)
    buffer.write(SPACE_BYTE)
    buffer.write(version.bytes)
    buffer.write(Newline)
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

  
}

class QueryParameters(str: String) {

  private val keyVals = str.split("&").toSeq.map{s => 
    val p = s.split("=")
    (p(0), p(1))
  }

  def firstValue(key: String): Option[String] = keyVals.collectFirst { case (k, v) if k == key => v }

  def allValues(key: String): Seq[String] = keyVals.collect { case (k, v) if k == key => v }

  def contains(key: String): Boolean = keyVals.exists { case (k, v) => k == key }

}

//firstLine does not include newline
class ParsedHttpRequest(val firstLine: Array[Byte], val headers: Headers, val body: Body) extends HttpRequest {
  def urlBytes = url.getBytes
  private def urlStart = method.bytes.length + 1
  private def urlLength = firstLine.length - 9 - urlStart
  lazy val url = new String(firstLine, urlStart, urlLength)

  def version = if (firstLine(firstLine.length - 1) == '0'.toByte) HttpVersion.`1.0` else HttpVersion.`1.1`
  lazy val  method = {
    import Method._
    def fail = throw new Exception(s"Invalid http method")
    firstLine(0) match {
      case 'G' => Get
      case 'P' =>
        firstLine(1) match {
          case 'A' => Patch
          case 'O' => Post
          case 'U' => Put
          case _   => fail
        }
      case 'D'   => Delete
      case 'H'   => Head
      case 'O'   => Options
      case 'T'   => Trace
      case 'C'   => Connect
      case other => fail
    }
  }

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

case class BasicHttpRequest(method: Method, url: String, headers: Headers, body: Body) extends HttpRequest {
  def urlBytes = url.getBytes
  def version = HttpVersion.`1.1`

  def firstLine = s"${method.name} $url ${version.stringValue}\r\n".getBytes

}

object HttpRequest {
  def get(url: String) = BasicHttpRequest(Method.Get, url, Headers.Empty, Body.Empty)
}
