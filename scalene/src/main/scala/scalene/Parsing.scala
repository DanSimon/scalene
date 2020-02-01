//Portions of this derived from
//https://github.com/tumblr/colossus/blob/master/colossus/src/main/scala/colossus/util/Parsing.scala
//Copyright (c) 2016 Tumblr and other contributors

package scalene

import java.nio.ByteBuffer

trait FastArrayBuilding[T] {

  def initSize: Int
  def shrinkOnComplete: Boolean
  def onComplete(buf: ReadBuffer): T

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

  final def write(buffer: ReadBuffer, bytes: Int) {
    while (writePos + bytes > build.length) {
      grow()
    }
    buffer.readInto(build, writePos, bytes)
    writePos += bytes
  }

  final def write(buffer: ReadBuffer) {
    write(buffer, buffer.bytesRemaining)
  }

  final def write(bytes: Array[Byte]) {
    while (writePos + bytes.length > build.length) {
      grow()
    }
    System.arraycopy(bytes, 0, build, writePos, bytes.length)
    writePos += bytes.length
  }

  final def complete(): T = {
    val res = onComplete(ReadBuffer(ByteBuffer.wrap(build, 0, writePos)))
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

trait LineParser extends FastArrayBuilding[Boolean] {
  private val CR = '\r'.toByte
  private val LF = '\n'.toByte


  def includeNewline: Boolean

  def shrinkOnComplete = false

  var scanByte = CR

  @inline private final def checkLineFeed(buffer: ReadBuffer): Boolean = {
    val b = buffer.buffer.get
    if (b == LF) {
      if (includeNewline) {
        write(CR)
        write(LF)
      }
      scanByte = CR
      complete()
    } else {
      throw new Exception("Malformed newline, expected \\r, got '$b'")
    }
  }

  //TODO : should return something instead of Int to indicate chunked body or body until EOS
  final def parse(buffer: ReadBuffer): Boolean = {
    var remaining = buffer.bytesRemaining
    if (scanByte == LF && remaining != 0) {
      if (checkLineFeed(buffer)) return true
      remaining -= 1
    }
    while (remaining != 0) {
      val byte = buffer.buffer.get
      remaining -= 1
      if (byte == CR) {
        if (remaining != 0) {
          if (checkLineFeed(buffer)) return true
          remaining -= 1
        } else {
          //this would only happen if the \n is in the next packet/buffer,
          //very rare but it can happen, but we can't complete until we've read it in
          scanByte = LF
        }
      } else {
        write(byte)
      }
    }
    false
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


