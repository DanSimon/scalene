package scalene.actor

import org.scalatest._
import scala.concurrent.{Promise, Future}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}


class ActorSpec extends FlatSpec with Matchers with BeforeAndAfterAll{

  behavior of "Actor"
  
  val pool = new Pool

  override def afterAll() {
    println("shutting")
    pool.shutdown()
    pool.join
  }

  it should "test" in {
    assert(true)
  }

  it should "basic actor" in {
    implicit val d = pool.createDispatcher("test")

    val b = new AtomicBoolean(false)

    val s = SimpleReceiver[String]{s => 
      if (s == "hey") {
        b.set(true)
      }
    }

    s.send("hey")
    Thread.sleep(200)
    assert(b.get)
  }

  it should "two actors on same dispatcher" in {
    implicit val d = pool.createDispatcher("l")
    val res = new AtomicBoolean(false)
    val a = SimpleReceiver[Unit]{_ => res.set(true)}

    val b = SimpleReceiver[Unit]{_ => a.send(())}

    Thread.sleep(50)
    b.send(())
    Thread.sleep(50)
    assert(res.get)


  }


  behavior of "Receiver"

  it should "catch exception and restart" in {

    implicit val d = pool.createDispatcher("asdf")
    val r = new AtomicBoolean(false)

    val a = d.attach(ctx => new Receiver[String](ctx) {
      def receive(s: String) : Unit = s match {
        case "DIE" => throw new Exception("BYE")
        case _ => r.set(true)
      }

      override def onBeforeRestart(ex: Exception) {
        r.set(false)
      }
    })

    a.send("HEY")
    Thread.sleep(50)
    assert(r.get)
    a.send("DIE")
    Thread.sleep(50)
    assert(!r.get)
    a.send("HEY AGAIN")
    Thread.sleep(50)
    assert(r.get)
  }






}

