//Portions of this derived from 
//https://github.com/tumblr/colossus/blob/master/colossus/src/main/scala/colossus/core/WriteBuffer.scala
//Copyright (c) 2016 Tumblr. Inc and other contributors

package scalene

import java.nio.ByteBuffer

trait WriteBuffer {

  def isOverflowed: Boolean

  protected def into(bytesNeeded: Int): ByteBuffer

  @inline final def write(from: ReadBuffer) {
    into(from.bytesRemaining).put(from.buffer)
  }

  @inline final def write(bytes: Array[Byte]) {
    if (bytes.length > 0) {
      into(bytes.length).put(bytes)
    }
  }

  @inline final def write(bytes: Array[Byte], offset: Int, length: Int) {
    into(length).put(bytes, offset, length)
  }

  @inline final def write(byte: Byte) {
    into(1).put(byte)
  }

  @inline final def write(char: Char) {
    write(char.toByte)
  }

  @inline final def write(number: Int) {
    if (number == 0) {
      write('0'.toByte)
    } else {
      val arr   = new Array[Byte](10)
      var r     = number
      var index = 9
      while (r > 0) {
        val q = (r * 3435973837L >> 35).toInt // divide positive int by 10
        arr(index) = (48 + r - (q << 3) - (q << 1)).toByte
        index -= 1
        r = q
      }
      write(arr, index + 1, 9 - index)
    }
  }

}

trait ReadWriteBuffer extends WriteBuffer {

  def size: Int
  def data: ReadBuffer

  def isEmpty = size == 0

}

class WriteBufferImpl(baseSize: Int, allocateDirect: Boolean = true) extends ReadWriteBuffer {

  private val base = if (allocateDirect) {
    ByteBuffer.allocateDirect(baseSize)
  } else {
    ByteBuffer.allocate(baseSize)
  }

  private var current: ByteBuffer = base

  @inline final def size = current.position

  @inline final def isOverflowed: Boolean = current != base

  @inline final protected def into(bytesNeeded: Int): ByteBuffer = {
    if (bytesNeeded > current.remaining) {
      val temp = current
      current = ByteBuffer.allocate((current.position() + bytesNeeded).toInt * 2)
      temp.flip
      current.put(temp)
    }
    current
  }

  def reset() {
    current = base
    base.clear()
  }

  final def data = {
    current.flip
    ReadBuffer(current)
  }
}
