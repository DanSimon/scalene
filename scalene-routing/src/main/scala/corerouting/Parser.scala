package scalene.corerouting

import scalene.Deferred

trait Clonable[T] {
  def cclone : T
}

trait ParserContainer[In <: Clonable[In], Out] { self: RoutingSuite[In, Out] =>

  trait Parser[I, O] {
    def parse(input: I) : Result[O]

    def map[U](f : O => U): Parser[I,U] = {
      val me = this
      new Parser[I,U] {
        def parse(input: I): Result[U] = me.parse(input).map(f)
        override def document(d: DocType): DocType = me.document(d)
      }
    }


    def filter(f: O => Boolean, failureMessage: String = "failed filter"): Parser[I,O] = {
      val me = this
      new Parser[I,O] {
        def parse(input:I): Result[O] = me.parse(input).filterOrElse(f, ParseError(ErrorReason.BadRequest, () => failureMessage))
        override def document(d: DocType): DocType = me.document(d)
      }
    }

    def document(d: DocType): DocType = d
  }

  trait Formatter[T] extends Parser[String,T]{
    def format(data: String): Result[T]

    def parse(input: String): Result[T] = format(input)
  }

  object StringF extends Formatter[String] {
    def format(data: String) = Right(data)
  }
  object IntF extends Formatter[Int] {
    def format(data: String) = try { Right(data.toInt) } catch {
      case e : Throwable => Left(ParseError(ErrorReason.BadRequest, () => e.getMessage))
    }
  }
  object BooleanF extends Formatter[Boolean] {
    def format(data: String) = 
      if (data.toLowerCase == "true") Right(true) 
      else if (data.toLowerCase == "false") Right(false)
      else Left(ParseError(ErrorReason.BadRequest, () => s"expected true/false, got '${data}'"))
  }



  trait Filter[I,O] {
    def apply(input: I): Deferred[O]
  }
  object Filter {
    def apply[I,O](f: I => Deferred[O]) = new Filter[I,O] {
      def apply(input: I) = f(input)
    }
  }
}
