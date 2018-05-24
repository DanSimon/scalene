package scalene

import java.nio.ByteBuffer

trait FastArrayBuilding {

  def initSize: Int
  def shrinkOnComplete: Boolean

  private var build: Array[Byte] = new Array[Byte](initSize)

  private var writePos = 0

  def written = writePos

  @inline final private def grow() {
    val nb = new Array[Byte](build.length * 2)
    System.arraycopy(build, 0, nb, 0, build.length)
    build = nb
  }

  @inline final def write(b: Byte) {
    if (writePos == build.length) {
      grow()
    }
    build(writePos) = b
    writePos += 1
  }

  def write(buffer: ReadBuffer, bytes: Int) {
    while (writePos + bytes > build.length) {
      grow()
    }
    buffer.takeInto(build, writePos, bytes)
    writePos += bytes
  }

  def write(bytes: Array[Byte]) {
    while (writePos + bytes.length > build.length) {
      grow()
    }
    System.arraycopy(bytes, 0, build, writePos, bytes.length)
    writePos += bytes.length
  }

  def complete[T](f: ReadBuffer => T): T = {
    val res = f(ReadBuffer(ByteBuffer.wrap(build, 0, writePos)))
    writePos = 0
    if (shrinkOnComplete && build.length > initSize) {
      build = new Array(initSize)
    }
    res
  }
}

object BodyCode {
  val HEAD_CONTINUE = -1
}
import BodyCode._

class LineParser(constructor: ReadBuffer => Int, includeNewline: Boolean = false, internalBufferBaseSize: Int = 100)
    extends FastArrayBuilding {
  private val CR = '\r'.toByte
  private val LF = '\n'.toByte

  def initSize         = internalBufferBaseSize
  def shrinkOnComplete = false

  var scanByte = CR

  private final def checkLineFeed(buffer: ReadBuffer): Int = {
    val b = buffer.data.get
    if (b == LF) {
      if (includeNewline) {
        write(CR)
        write(LF)
      }
      scanByte = CR
      complete[Int](constructor)
    } else {
      throw new Exception("Malformed newline, expected \\r, got '$b'")
    }
  }

  //TODO : should return something instead of Int to indicate chunked body or body until EOS
  final def parse(buffer: ReadBuffer): Int = {
    var bodySize = HEAD_CONTINUE
    if (scanByte == LF && buffer.hasUnreadData) {
      bodySize = checkLineFeed(buffer)
    }
    while (buffer.hasUnreadData && bodySize == BodyCode.HEAD_CONTINUE) {
      val byte = buffer.data.get
      if (byte == CR) {
        if (buffer.hasUnreadData) {
          bodySize = checkLineFeed(buffer)
        } else {
          //this would only happen if the \n is in the next packet/buffer,
          //very rare but it can happen, but we can't complete until we've read it in
          scanByte = LF
        }
      } else {
        write(byte)
      }
    }
    bodySize
  }

}

class HttpParser(headLineProcessor: ReadBuffer => Int, bodyProcessor: ReadBuffer => Unit) extends FastArrayBuilding {

  var parsingHead = true
  var bodySize = 0

  val headParser = new LineParser(headLineProcessor, false, 100)
  val zeroBody = ReadBuffer(ByteBuffer.wrap(new Array[Byte](0)))

  def initSize = 1024
  def shrinkOnComplete = true

  def parse(buffer: ReadBuffer): Unit = {
    while (parsingHead && buffer.hasUnreadData) {
      bodySize = headParser.parse(buffer)
      if (bodySize == 0) {
        bodyProcessor(zeroBody)        
      } else if (bodySize > 0) {
        parsingHead = false        
      }
    } 
    if (!parsingHead) {
      val amountToTake = math.min(bodySize, buffer.remaining)
      write(buffer, amountToTake)
      bodySize -= amountToTake
      if (bodySize == 0) {
        complete[Unit](bodyProcessor)
        parsingHead = true
      }
    }
  }
}

object ParsingUtils {

  def caseInsensitiveSubstringMatch(candidate: Array[Byte], substringLower: Array[Byte]): Boolean = {
    var i = 0
    if (substringLower.length > candidate.length) {
      false
    } else {        
      while (i < substringLower.length && (candidate(i) == substringLower(i) || candidate(i) + 32 == substringLower(i))) {
        i += 1
      }
      i == substringLower.length
    }
  }


}

