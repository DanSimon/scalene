package scalene
package http

import java.util.{Arrays, LinkedList}

import HttpParsing._

trait HttpRequest extends HttpMessage {
  def method: Method
  def url: String

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

  
}

class ParsedHttpRequest(val firstLine: Array[Byte], val headers: LinkedList[Header], val body: Body) extends HttpRequest {
  def url = ???
  def urlBytes = ???
  def version = ???
  def method = ???

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

case class BasicHttpRequest(method: Method, url: String, headers: LinkedList[Header], body: Body) extends HttpRequest {
  def urlBytes = url.getBytes
  def version = HttpVersion.`1.1`

  def firstLine = s"${method.name} $url ${version.stringValue}\r\n".getBytes

}

object HttpRequest {
  def noHeaders = new LinkedList[Header]()
  def get(url: String) = BasicHttpRequest(Method.Get, url, noHeaders, Body.Empty)
}
