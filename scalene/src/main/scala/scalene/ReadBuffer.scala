//Portions of this derived from
//https://github.com/tumblr/colossus/blob/master/colossus/src/main/scala/colossus/core/DataBuffer.scala
//Copyright (c) 2017 Tumblr and other contributors

package scalene

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel


case class ReadBuffer(buffer: ByteBuffer) extends AnyVal {

  def next(): Byte = buffer.get

  def read(n: Int): Array[Byte] = {
    val actualSize = math.min(bytesRemaining, n)
    val arr        = new Array[Byte](actualSize)
    buffer.get(arr)
    arr
  }

  def readAll: Array[Byte] = read(bytesRemaining)

  def readCopy: ReadBuffer = ReadBuffer(ByteBuffer.wrap(readAll))

  def readInto(target: Array[Byte], offset: Int, length: Int) {
    if (length > bytesRemaining) {
      throw new IndexOutOfBoundsException(
        s"Attempted to read $length bytes from buffer with only $bytesRemaining bytes remaining")
    }
    if (offset + length > target.length) {
      throw new IndexOutOfBoundsException("Attempted to write too many byte to target array")
    }
    buffer.get(target, offset, length)
  }

  def peekCopy(): Array[Byte] = {
    buffer.mark()
    val data = readAll
    buffer.reset()
    data
  }

  def skip(n: Int) = {
    buffer.position(buffer.position() + n)
  }

  def skipAll() {
    skip(bytesRemaining)
  }

  def writeTo(channel: SocketChannel) = {
    channel.write(buffer)
  }

  def hasNext = buffer.hasRemaining

  def isEmpty = !hasNext

  def bytesRemaining = buffer.remaining()

  def bytesRead = buffer.position()

  def size = buffer.limit()

  def clear() = buffer.clear()

  def readString = new String(readAll)

}

object ReadBuffer {
  def apply(buffer: ByteBuffer, bytesRead: Int): ReadBuffer = {
    val n = buffer.asReadOnlyBuffer
    n.limit(bytesRead)
    ReadBuffer(n)
  }

  def apply(data: Array[Byte]): ReadBuffer = ReadBuffer(ByteBuffer.wrap(data))

  def apply(str: String): ReadBuffer = apply(str.getBytes("UTF-8"))

}

