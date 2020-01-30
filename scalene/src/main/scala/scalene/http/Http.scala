package scalene.http

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.{Arrays, Date, LinkedList, Locale, TimeZone}

import scalene.actor.Pool
import scalene._
import scalene.stream._
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

  override def toString = name
  override def equals(that: Any) = that match {
    case m: Method if Arrays.equals(m.bytes, bytes) => true
    case _ => false
  }

}

object Method {
  val Connect   = new Method("CONNECT")
  val Delete    = new Method("DELETE")
  val Get       = new Method("GET")
  val Head      = new Method("HEAD")
  val Options   = new Method("OPTIONS")
  val Patch     = new Method("PATCH")
  val Post      = new Method("POST")
  val Put       = new Method("PUT")
  val Trace     = new Method("TRACE")
}

case class ResponseCode(code: Int, codeMessage: String) {
  val v0FirstLine = s"HTTP/1.0 $code $codeMessage\r\n".getBytes
  val v1FirstLine = s"HTTP/1.1 $code $codeMessage\r\n".getBytes
}

object ResponseCode {
  val Ok = ResponseCode(200, "OK")
  val BadRequest = ResponseCode(400, "BAD REQUEST")
  val NotFound = ResponseCode(404, "NOT FOUND")
  val Error = ResponseCode(500, "ERROR")

  val codes = Seq(
    Ok,
    BadRequest,
    NotFound,
    Error
  ).map{c => (c.code, c)}.toMap

  def apply(intCode: Int): ResponseCode = codes(intCode)

}



trait HttpMessage {
  def headers: Headers
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

