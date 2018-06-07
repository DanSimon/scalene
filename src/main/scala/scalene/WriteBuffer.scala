package scalene

import java.nio.ByteBuffer

/**
 * An elastic buffer with a fixed-sized primary buffer and a dynamic seconday buffer meant for overflow.
  */
trait WriteBuffer {

  def isOverflowed: Boolean

  protected def copyDestination(bytesNeeded: Long): ByteBuffer

  final def write(from: ReadBuffer) {
    copyDestination(from.remaining).put(from.data)
  }

  final def write(bytes: Array[Byte]) {
    if (bytes.length > 0) {
      copyDestination(bytes.length).put(bytes)
    }
  }

  final def write(bytes: Array[Byte], offset: Int, length: Int) {
    copyDestination(length).put(bytes, offset, length)
  }

  final def write(byte: Byte) {
    copyDestination(1).put(byte)
  }

  final def write(char: Char) {
    write(char.toByte)
  }

  final def write(number: Int) {
    if (number == 0) {
      write('0'.toByte)
    } else {
      val arr   = new Array[Byte](10)
      var r     = number
      var index = 9
      while (r > 0) {
        arr(index) = ((r % 10) + 48).toByte
        r = r / 10
        index -= 1
      }
      write(arr, index + 1, 10 - (index + 1))
    }
  }

  /*
  def write(block: DataBlock) {
    write(block.data)
  }
  */

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

  private var dyn: Option[ByteBuffer] = if (allocateDirect) None else Some(base)

  final def size = if (dyn.isDefined) dyn.get.position() else base.position

  final def isOverflowed: Boolean = dyn.isDefined

  final private def dynAvailable = dyn.map { _.remaining }.getOrElse(0)

  final private def growDyn() {
    dyn match {
      case Some(old) => {
        val nd = ByteBuffer.allocate(old.capacity * 2)
        old.flip
        nd.put(old)
        dyn = Some(nd)
      }
      case None => {
        val nd = ByteBuffer.allocate(baseSize * 2)
        base.flip
        nd.put(base)
        dyn = Some(nd)
      }
    }
  }

  final protected def copyDestination(bytesNeeded: Long): ByteBuffer = {
    if (base.remaining >= bytesNeeded) base
    else {
      while (dynAvailable < bytesNeeded) {
        growDyn()
      }
      dyn.get
    }
  }

  def reset() {
    dyn = None
    base.clear()
  }

  final def data = {
    val d = dyn.getOrElse(base)
    d.flip
    ReadBuffer(d)
  }
}
