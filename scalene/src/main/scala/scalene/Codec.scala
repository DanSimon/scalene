package scalene

trait MessageDecoder[T] {
  def decode(data: ReadBuffer)

  def decodeAll(data: ReadBuffer) {
    while (!data.isEmpty) {
      decode(data)
    }
  }

  def messagesDecoded: Long
  def currentMessageSize: Long



  def endOfStream()
}

trait MessageEncoder[T] {
  def encode(message: T, buffer: WriteBuffer): Option[stream.Stream[Writable]]
}


trait Codec[I,O] extends MessageDecoder[I] with MessageEncoder[O]

object Codec {
  type Factory[I,O] = (I => Unit) => Codec[I,O]
}

/*
 * An Writable is any object that is able to write itself directly to a
 * WriteBuffer in one call.  So a Stream cannot be a Writable, but the individual items in a stream can.  
 *
 * In most cases a Writable will just be a ReadBuffer or Byte Array, but it
 * could also be something like a sequence of arrays, in which case we can
 * avoid unnecessary copying of the sequence into a single array
 */
trait Writable extends Any{
  def writeTo(buffer: WriteBuffer)
}
