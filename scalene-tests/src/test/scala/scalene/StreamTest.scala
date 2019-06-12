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

    def onClose() {
      closed = true
    }

    def onError(reason: Throwable): Unit = {
      collectedError = Some(reason)
    }
  }

  trait MockPartialSink[T] extends PartialSink[T] {
    val signal = new LiveSignal
    val items = collection.mutable.ArrayBuffer[T]()

    var allow = false

    def attemptPush(item: T): PartialPushResult = if (allow) {
      items.append(item)
      PushResult.Ok
    } else {
      PartialPushResult.WaitRejected(signal)
    }
  }

  class MockBufferedCollector[T] extends BufferedSink[T] with MockPartialSink[T] {
    var closed = false
    def onClose(): Unit = {
      closed = true
    }

    def onError(reason: Throwable): Unit = {

    }
  }


  it should "fromIter" in {
    val c = new TestCollector[Int]
    Stream.fromIter(List(1, 2, 3).toIterator).complete(c)
    c.collected.toList shouldBe List(1, 2, 3)
  }

  behavior of "BufferedSink"

  it should "properly buffer" in {
    val sink = new MockBufferedCollector[String]
    sink.signal.isFilled shouldBe false
    sink.signal.isSignaled shouldBe false
    val res = sink.push("a1")
    res.isInstanceOf[PushResult.WaitAccepted] shouldBe true
    val waitSignal = res.asInstanceOf[PushResult.WaitAccepted].signal
    sink.push("a2").isInstanceOf[PushResult.WaitAccepted] shouldBe true
    sink.push("a3").isInstanceOf[PushResult.WaitAccepted] shouldBe true
    waitSignal.isSignaled shouldBe false
    sink.items.isEmpty shouldBe true

    sink.allow = true
    sink.signal.signal()

    waitSignal.isSignaled shouldBe true
    sink.items.toList shouldBe List("a1", "a2" , "a3")
  }

  it should "handle being closed with buffered items" in {
    val sink = new MockBufferedCollector[String]
    sink.push("a1").isInstanceOf[PushResult.WaitAccepted] shouldBe true
    sink.push("a2").isInstanceOf[PushResult.WaitAccepted] shouldBe true
    sink.push("a3").isInstanceOf[PushResult.WaitAccepted] shouldBe true
    sink.closed shouldBe false
    sink.close()
    sink.closed shouldBe false
    sink.allow = true
    sink.signal.signal()
    sink.items.toList shouldBe List("a1", "a2" , "a3")
    sink.closed shouldBe true
  }




}


