package scalene.routing

import scalene._
import scalene.http._
import scalene.httprouting._
import org.scalatest._

class CListTest extends WordSpec with MustMatchers {

  implicit val intResponse = new AsResponse[Int, HttpResponse] {
    def apply(i: Int) = Deferred.successful(Body.plain(i.toString).ok)
  }

  "syntax tests" must {
    "work" in {
      val a = GET / "foo" / ![Int] / "bar" / ![String] to {case (i,a) => (i + 1)}

      val b = GET / "foo"

      b.isInstanceOf[ExactMatchPath] must equal(true)
    }
  }

}

