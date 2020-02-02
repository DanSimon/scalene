package scalene.http

import java.nio.ByteBuffer
import java.text.SimpleDateFormat
import java.util.{Arrays, Date, LinkedList, Locale, TimeZone}

import scalene.actor.Pool
import scalene._
import scalene.stream._
import scalene.util._


case class Body(data: BodyData, contentType: Option[ContentType]) {

}

sealed trait BodyData {
  def collect(): Deferred[ReadBuffer]
}

object BodyData {
  case class Static(data: Array[Byte]) extends BodyData {
    def collect(): Deferred[ReadBuffer] = Deferred.successful(ReadBuffer(data))
  }
  case class Stream(data: scalene.stream.Stream[ReadBuffer]) extends BodyData {
    def collect(): Deferred[ReadBuffer] = data.complete(new BodyCollector)
  }

  val Empty = Static(new Array[Byte](0))
}

trait BodyFormatter[-T] {
  def apply(item: T): Body
}

object BodyFormatter {

  implicit val nop = new BodyFormatter[Body] {
    def apply(b: Body) = b
  }

}

trait SimpleBodyFormatter[-T] extends BodyFormatter[T] {
  def format(item: T): BodyData
  def contentType: Option[ContentType]

  def apply(item: T): Body = Body(format(item), contentType)
}

object Body {

  def apply(data: Array[Byte], contentType: Option[ContentType]): Body = Body(BodyData.Static(data), contentType)

  val Empty = Body(BodyData.Empty, None)

  def plain(str: String) = Body(str.getBytes, Some(ContentType.`text/plain`))
  def json(encoded: String) = Body(encoded.getBytes, Some(ContentType.`application/json`))
}
