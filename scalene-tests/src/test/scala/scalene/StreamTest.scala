package scalene
package stream

import org.scalatest._

class StreamTest extends AsyncFlatSpec with Matchers with BeforeAndAfterAll{

  behavior of "Stream"

  class TestCollector[T] extends Sink[T] {

    val collected = collection.mutable.ArrayBuffer[T]()
    var closed: Boolean = false
    var collectedError: Option[Throwable] = None

    def push(item: T): PushResult = {
      collected.append(item)
      PushResult.Ok
    }

    def close() {
      closed = true
    }

    def error(reason: Throwable): Unit = {
      collectedError = Some(reason)
    }
  }


  it should "fromIter" in {
    val c = new TestCollector[Int]
    Stream.fromIter(List(1, 2, 3).toIterator).complete(c)
    c.collected.toList shouldBe List(1, 2, 3)
  }

}


