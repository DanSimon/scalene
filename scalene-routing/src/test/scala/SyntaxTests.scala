package scalene.routing

import scalene._
import scalene.http._
import scalene.httprouting._
import org.scalatest._

class CListTest extends WordSpec with MustMatchers {

  implicit val intResponse = new AsResponse[Int, HttpResponse] {
    def apply(i: Int) = Deferred.successful(Body.plain(i.toString).ok)
  }
  implicit val stringResponse = new AsResponse[String, HttpResponse] {
    def apply(s: String) = Deferred.successful(Body.plain(s).ok)
  }

  "syntax tests" must {
    "work" in {
      val a = GET / "foo" / ![Int] / "bar" / ![String] to {case (i,a) => intResponse(i + 1)}

      val parserToOps = GET to {_ => stringResponse("yay")}

      val b = GET / "foo"
      b.isInstanceOf[ExactMatchPath] must equal(true)

      b to {_ => stringResponse("yay")}
    }
  }

}

