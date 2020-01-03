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


}

