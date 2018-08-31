package scalene.corerouting

trait ExtractionContainer[In <: Clonable[In], Out]{ self: RoutingSuite[In, Out] =>

/**
 * An extraction defines what should be extracted, but not how it is
 * extracted.  The `A` type determines which parser should be used, and `B`
 * is the "final" type, representing possible map operations
 */
trait Extraction[A,B] {

  def name: String

  def extraction[I](parser: Parser[I,A]): Parser[I,B]

  def map[C](f: B => C): Extraction[A,C] = {
    val me = this
    new Extraction[A,C] {
      def name = me.name
      def extraction[I](parser: Parser[I,A]): Parser[I,C] = me.extraction(parser).map(f)
    }
  }

  def filter(f: B => Boolean, failureResponse: String = "error" ): Extraction[A,B] = {
    val me = this
    val formattedResponse = failureResponse.replaceAll("\\{NAME\\}", name)
    new Extraction[A,B] {
      def name = me.name
      def extraction[I](parser: Parser[I,A]): Parser[I,B] = me.extraction(parser).filter(f, formattedResponse)
    }
  }

}

case class IdentityExtraction[T](name: String) extends Extraction[T, T] {
  def extraction[I](parser: Parser[I, T]) = parser
}

}
