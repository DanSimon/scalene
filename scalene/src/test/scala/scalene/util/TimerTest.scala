  package scalene.util

import scalene.actor._
import org.scalatest.{Timer => _, _}
import scala.concurrent.{Future, Promise}

class TimerSpec extends AsyncFlatSpec with Matchers with BeforeAndAfterAll{

  behavior of "Timer"
  
  val pool = new Pool

  override def afterAll() {
    pool.shutdown()
  }

  it should "work" in {
    implicit val d = pool.createDispatcher("asdf")
    val promise = Promise[String]()
    val timer = new Timer
    SimpleReceiver[String](str => timer.schedule(100){promise.success(str)}).send("done")
    val start = System.currentTimeMillis
    promise.future.map{s => assert(s == "done" && System.currentTimeMillis >= start + 90)}
  }

}

