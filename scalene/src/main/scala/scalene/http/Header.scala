package scalene.http

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.{Arrays, Date, LinkedList, Locale, TimeZone}

import scalene.actor.Pool
import scalene._
import scalene.stream._
import scalene.util._

import HttpParsing._

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
  def value = sdf.format(new Date(lastUpdated))

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

case class EncodedHeader(key: HeaderKey, value: Writable) extends Writable {
  def writeTo(buffer: WriteBuffer): Unit = {
    buffer.write(key.bytes)
    buffer.write(Header.KeyValueSeparatorBytes)
    value.writeTo(buffer)
    buffer.write(HttpParsing.Newline)
  }
}

object Header {
  def apply(key: String, value: String) : Header = new StaticHeader(key, value)

  val KeyValueSeparatorBytes: Array[Byte] = ": ".getBytes()
}
