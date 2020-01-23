package scalene.routing

import scalene.http.{Body, HttpResponse}
import scalene.corerouting.AsResponse
import org.scalatest._
import BasicConversions._

class ParserTest extends WordSpec with MustMatchers {

  "Extraction" must {
    "basic" in {
      val parser: Parser[String, Int] = IntF
      val extraction = ![Int]

      extraction.extraction(parser).parse("4") must equal(Right(4))
    }

    "map" in {
      val parser: Parser[String, Int] = IntF
      val extraction = ![Int].map{_ + 1}

      extraction.extraction(parser).parse("4") must equal(Right(5))
    }

    "filter" in {
      val parser: Parser[String, Int] = IntF
      val extraction = ![Int].filter(_ == 4, "not4")

      extraction.extraction(parser).parse("4").isLeft must equal(false)
      extraction.extraction(parser).parse("5").isLeft must equal(true)
    }

    "recover" in {
      val parser: Parser[String, Int] = IntF
      val extraction = ![Int].filter(_ == 4, "not4").recover(_ => 1)

      extraction.extraction(parser).parse("5") must equal(Right(1))
    }

    "optional" in {
      val parser: Parser[String, Int] = IntF
      val extraction = ![Int].filter(_ == 4, "not4")

      val opt = optional(extraction)

      opt.extraction(parser).parse("4") must equal(Right(Some(4)))
      opt.extraction(parser).parse("5") must equal(Right(None))
    }


  }


}


