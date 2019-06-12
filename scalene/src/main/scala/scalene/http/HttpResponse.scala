package scalene
package http

import java.util.LinkedList
import HttpParsing._

trait HttpResponse extends HttpMessage {
  def code: ResponseCode

  def encodeFirstLine(buffer: WriteBuffer): Unit = {
    buffer.write(code.v1FirstLine)
  }
}

class ParsedHttpResponse(firstLine: Array[Byte], val headers: Headers, val body: Body) extends HttpResponse {
  lazy val code = ResponseCode(
    (firstLine(CODE_START) - '0'.toInt) * 100 
    + (firstLine(CODE_START + 1) - '0'.toInt) * 10  
    + (firstLine(CODE_START + 2) - '0'.toInt)
  )
  lazy val version = HttpVersion.`1.1` //TODO FIX
}

case class BasicHttpResponse(code: ResponseCode, headers: Headers, body: Body) extends HttpResponse {
  def version = HttpVersion.`1.1`
}

object HttpResponse {
  private val emptyHeaders = new LinkedList[Header]()
  def apply(code: ResponseCode, body:Body): HttpResponse = BasicHttpResponse(code, Headers.Empty, body)
}

trait ResponseBuilding {

  implicit class BuildToHttpResponse[T](item: T)(implicit builder: BodyFormatter[T]) {

    def withCode(code: ResponseCode) : BasicHttpResponse = BasicHttpResponse(code, Headers.Empty, builder(item))

    def ok = withCode(ResponseCode.Ok)
    def notFound = withCode(ResponseCode.NotFound)
    def error = withCode(ResponseCode.Error)
  }

}
