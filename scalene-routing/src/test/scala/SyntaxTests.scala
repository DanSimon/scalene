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

  "ExactMatchPath" must {
    "work on it's own" in {
      val expected = ExactMatchPath(scalene.http.Method.Get, ConstantPrefixPath("foo" :: Nil))
      GET / "foo" must equal(expected)
    }

    "combine with string" in {
      val a = ExactMatchPath(scalene.http.Method.Get, ConstantPrefixPath("foo" :: Nil))
      val expected = ExactMatchPath(scalene.http.Method.Get, ConstantPrefixPath("foo" :: "bar" :: Nil))
      a / "bar" must equal(expected)
    }

    "combine with path extractor" in {
      val route = GET / "foo" / ![Int]
      //implicit val builder: RouteBuilderCombiner[RouteBuilder[Int], Parser[RequestContext, Unit]] = RouteBuilderCombiner.builderCom[Int, Unit, Parser[RequestContext, Unit], Int]
      val more = route + Parameter("foo", "bar")

      more.isInstanceOf[RouteBuilder[Int]] must equal(true)
    }

    "combine with other extractor" in {

      
      val route = GET / "foo"
      val p: Parser[RequestContext, Int] = Parameter("foo", ![Int])

      val more = route + ?("foo", ![Int])
    }

    "combine with wildcard" in {
      val route = GET / "foo" / *

      val route2 = GET / "foo" / !*

      route2 to {str => str.toUpperCase.ok}
    }
  }

}

