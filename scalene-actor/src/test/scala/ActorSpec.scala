package scalene.actor

import org.scalatest._
import scala.concurrent.{Promise, Future}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}


class ActorSpec extends AsyncFlatSpec with Matchers with BeforeAndAfterAll{

  behavior of "Actor"
  
  val pool = new Pool

  override def afterAll() {
    pool.shutdown()
    pool.join
  }

  it should "two actors on same dispatcher" in {
    implicit val d = pool.createDispatcher
    val promise = Promise[String]()

    val a = SimpleReceiver[String]{s => promise.success(s)}

    val b = SimpleReceiver[String]{s => a.send(s)}

    Thread.sleep(200)

    b.send("test")

    promise.future.map{s => assert(s == "test")}
  }

  it should "handle wake locks" in {
    implicit val d = pool.createDispatcher
    val go = new AtomicBoolean(false)
    val started = new AtomicBoolean(false)
    val bstarted = new AtomicBoolean(false)

    case class Foo() extends NoWakeMessage

    val a = d.attach(ctx => new Receiver[Foo](ctx) {
      ctx.dispatcher.addWakeLock(new WakeLock {
        def wake(): Unit = {
          go.set(true)
        }
      })
      def receive(s: Foo) = {
        started.set(true)
        while (!go.get()) {
          Thread.sleep(50)
        }
      }
    })

    val b = SimpleReceiver[String]{s => bstarted.set(true)}

    a.send(Foo())
    while(!started.get()) {
      Thread.sleep(20)
    }
    go.get should equal(false)
    b.send("whatever")
    while (!bstarted.get()) {
      Thread.sleep(20)
    }
    go.get should equal(true)
  }

}

