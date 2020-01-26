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

    def recover(f: ParseError => O): Parser[I,O] = {
      val me = this
      new Parser[I,O] {
        def parse(input: I): Result[O] = me.parse(input) match {
          case r @ Right(parsed) => r
          case Left(uhoh) => try {
            Right(f(uhoh))
          } catch {
            case e: Exception => Left(ParseError.error(e.getMessage))
          }
        }
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

  val DefaultFormatterError: String => String = input => {
    val MAX_LEN = 100
    val max = if (input.length > MAX_LEN) input.substring(100) + "..." else input
    s"invalid value: '$max'"
  }

  def simpleFormatter[T](f: String => T, onFailureMessage: String => String = DefaultFormatterError) = new Formatter[T] {
    def format(data: String) = try { Right(f(data)) } catch {
      case e : Throwable => Left(ParseError(ErrorReason.BadRequest, () => onFailureMessage(data)))
    }
  }

  val IntF    = simpleFormatter[Int](_.toInt)
  val DoubleF = simpleFormatter[Double](_.toDouble)
  val FloatF  = simpleFormatter[Float](_.toFloat)
  val LongF   = simpleFormatter[Long](_.toLong)

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
