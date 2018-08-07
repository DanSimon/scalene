package router

import org.scalatest._
import shapeless._
import colossus.protocols.http._
import scala.concurrent.Future

class CListTest extends AsyncWordSpec with MustMatchers {

  def R = CList.empty[RequestContext]

  "CList" must {
    "properly merge clists" in {
      val a = R + Parameter("a", ![Int]) + Parameter("b", ![String])
      val b = R + Parameter("c", ![Boolean])

      val c: RequestContext => Future[Int :: String :: Boolean :: HNil] = (a + b).toSubRoute.toCompleteRoute

      c(new RequestContext(HttpRequest.get("/foo?a=3&b=test&c=true"))) map { case i :: s :: b :: HNil =>
        i mustBe 3
        s mustBe "test"
        b mustBe true
        succeed
      }

    }

  }


}
