package scalene

import java.nio.ByteBuffer

/**
 * An elastic buffer with a fixed-sized primary buffer and a dynamic seconday buffer meant for overflow.
  */
trait WriteBuffer {

  def isOverflowed: Boolean

  protected def copyDestination(bytesNeeded: Long): ByteBuffer

  def write(from: ReadBuffer) {
    copyDestination(from.remaining).put(from.data)
  }

  def write(bytes: Array[Byte]) {
    copyDestination(bytes.length).put(bytes)
  }

  def write(bytes: Array[Byte], offset: Int, length: Int) {
    copyDestination(length).put(bytes, offset, length)
  }

  def write(byte: Byte) {
    copyDestination(1).put(byte)
  }

  def write(char: Char) {
    write(char.toByte)
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

}

/**
  * A ByteBuffer-backed growable buffer.  A fixed-size direct buffer is used for
  * most of the time, but overflows are written to a eponentially growing
  * non-direct buffer.
  *
  *
  * Be aware, the DataBuffer returned from `data` merely wraps the underlying
  * buffer to avoid copying
  */
class WriteBufferImpl(baseSize: Int, allocateDirect: Boolean = true) extends ReadWriteBuffer {

  private val base = if (allocateDirect) {
    ByteBuffer.allocateDirect(baseSize)
  } else {
    ByteBuffer.allocate(baseSize)
  }

  private var dyn: Option[ByteBuffer] = if (allocateDirect) None else Some(base)

  def size = dyn.map { _.position }.getOrElse(base.position)

  def isOverflowed: Boolean = dyn.isDefined

  private def dynAvailable = dyn.map { _.remaining }.getOrElse(0)

  private def growDyn() {
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

  protected def copyDestination(bytesNeeded: Long): ByteBuffer =
    if (base.remaining >= bytesNeeded) base
    else {
      while (dynAvailable < bytesNeeded) {
        growDyn()
      }
      dyn.get
    }

  def reset() {
    dyn = None
    base.clear()
  }

  def data = {
    val d = dyn.getOrElse(base)
    d.flip
    ReadBuffer(d)
  }
}
