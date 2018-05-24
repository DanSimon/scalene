package scalene


abstract class Codec[I,O](val onDecode: I => Unit) {
  def decode(data: ReadBuffer)
  def encode(message: O, buffer: WriteBuffer)

  def endOfStream()

  def decodeAll(data: ReadBuffer) {
    while (!data.isEmpty) {
      decode(data)
    }
  }
}

object Codec {
  type Factory[I,O] = (I => Unit) => Codec[I,O]
}
