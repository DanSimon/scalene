package scalene.util

import microactor._
import org.scalatest._
import scala.concurrent.{Future, Promise}

class TimerSpec extends AsyncFlatSpec with Matchers with BeforeAndAfterAll{

  behavior of "Timer"
  
  val pool = new Pool

  override def afterAll() {
    pool.shutdown()
  }

  it should "work" in {
    implicit val d = pool.createDispatcher
    val promise = Promise[String]()
    val timer = new Timer
    SimpleReceiver[String](str => timer.schedule(100){promise.success(str)}).send("done")
    val start = System.currentTimeMillis
    promise.future.map{s => assert(s == "done" && System.currentTimeMillis >= start + 90)}
  }

}

