package scalene

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel


trait Encoder extends Any {
  def encode(out: WriteBuffer)
}

/** A thin wrapper around a NIO ByteBuffer with data to read
  *
  * ReadBuffers are the primary way that data is read from and written to a
  * connection.  ReadBuffers are mutable and not thread safe.  They can only be
  * read from once and cannot be reset.
  *
  */
case class ReadBuffer(data: ByteBuffer) extends AnyVal with Encoder {

  /** Get the next byte, removing it from the buffer
    *
    * WARNING : This method will throw an exception if no data is left.  It is
    * up to you to use hasUnreadData to figure out if you should call this.
    * This is done to avoid unnecessary object allocation with using Option
    *
    * @return the next byte in the buffer
    */
  def next(): Byte = data.get

  def encode(out: WriteBuffer) {
    out.write(this)
  }

  /** Get some bytes
    * @param n how many bytes you want.
    * @return an filled array of size min(n, remaining)
    */
  def take(n: Int): Array[Byte] = {
    val actualSize = math.min(remaining, n)
    val arr        = new Array[Byte](actualSize)
    data.get(arr)
    arr
  }

  /** Returns an array containing all of the unread data in this Databuffer */
  def takeAll: Array[Byte] = take(remaining)

  /** Copy the unread data in this buffer to a new buffer
    *
    * Data will not be shared between the buffers.  The position of this buffer will be completed
    *
    * @return a new ReadBuffer containing only the unread data in this buffer
    */
  def takeCopy: ReadBuffer = ReadBuffer(ByteBuffer.wrap(takeAll))

  /** Directly copy data into a target byte array
    * @param buffer the array to copy into
    * @param offset the first index of buffer to start writing to
    * @param length how many bytes to write
    * @throws IndexOutOfBoundsException if target array is too small or buffer doesn't have sufficient bytes available
    */
  def takeInto(buffer: Array[Byte], offset: Int, length: Int) {
    if (length > remaining) {
      throw new IndexOutOfBoundsException(
        s"Attempted to take $length bytes from buffer with only $remaining bytes remaining")
    }
    if (offset + length > buffer.length) {
      throw new IndexOutOfBoundsException("Attempted to write too many byte to target array")
    }
    data.get(buffer, offset, length)
  }

  /** Skip over n bytes in the buffer.
    *
    * @param n the number of bytes to skip.
    * @throws IllegalArgumentException if n is larger than the number of remaining bytes
    */
  def skip(n: Int) = {
    data.position(data.position() + n)
  }

  def skipAll() {
    skip(remaining)
  }

  /** Write the buffer into a SocketChannel
    *
    * The buffer's taken and remaining values will be updated to reflect how
    * much data was written.  Be aware that buffer's containing large amounts of
    * data will probably not be written in one call
    *
    * @param channel the channel to write to
    * @return how many bytes were written
    */
  def writeTo(channel: SocketChannel) = {
    channel.write(data)
  }

  /** Returns true if this ReadBuffer still has unread data, false otherwise */
  def hasNext = data.hasRemaining

  /** Returns true if this ReadBuffer still has unread data, false otherwise */
  def hasUnreadData = data.hasRemaining

  def remaining = data.remaining

  /** Returns how many bytes have already been read from this ReadBuffer */
  def taken = data.position

  /** Returns the total size of this ReadBuffer */
  def size = data.limit

}

object ReadBuffer {
  def apply(buffer: ByteBuffer, bytesRead: Int): ReadBuffer = {
    val n = buffer.asReadOnlyBuffer
    n.limit(bytesRead)
    ReadBuffer(n)
  }

  def apply(data: Array[Byte]): ReadBuffer = ReadBuffer(ByteBuffer.wrap(data))

  def apply(data: String): ReadBuffer = apply(data.getBytes("UTF-8"))

}

