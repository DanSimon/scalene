package scalene.routing

import scalene.http.{Body, HttpResponse}
import scalene.corerouting.AsResponse
import org.scalatest._
import BasicConversions._

class CListTest extends WordSpec with MustMatchers {

  "syntax tests" must {
    "work" in {
      val a = GET / "foo" / ![Int] / "bar" / ![String] to {case (i,a) => (i + 1).ok}

      val parserToOps = GET to {_ => "yay".ok}

      val b = GET / "foo"
      b.isInstanceOf[ExactMatchPath] must equal(true)

      b to {_ => "yay".ok}

      val c = Parameter("a" , "va") + Parameter("b", "bv") as "hey".ok
    }
  }

  "doc tests" must {
    "work" in {
      (GET / "foo" / ![Int] as "whatever".ok).document.value.build must equal("GET /foo/<int>")
    }
  }

}

