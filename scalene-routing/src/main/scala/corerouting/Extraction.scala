package scalene.corerouting


/**
 * An extraction defines what should be extracted, but not how it is
 * extracted.  The `A` type determines which parser should be used, and `B`
 * is the "final" type, representing possible map operations
 */
trait Extraction[A,B] {

  def extraction[I](parser: Parser[I,A]): Parser[I,B]

  def map[C](f: B => C): Extraction[A,C] = {
    val me = this
    new Extraction[A,C] {
      def extraction[I](parser: Parser[I,A]): Parser[I,C] = me.extraction(parser).map(f)
    }
  }

  def filter(f: B => Boolean): Extraction[A,B] = {
    val me = this
    new Extraction[A,B] {
      def extraction[I](parser: Parser[I,A]): Parser[I,B] = me.extraction(parser).filter(f)
    }
  }

}

case class IdentityExtraction[T]() extends Extraction[T, T] {
  def extraction[I](parser: Parser[I, T]) = parser
}
