package scalene.routing


case class ParserDoc(pathSegments: List[String], method: String, headers: List[String], queryParams: List[String])
object ParserDoc {
  val empty = ParserDoc(Nil, "", Nil, Nil)
}

trait DocBuilder[T] {

  def apply(item: T, docs: ParserDoc): ParserDoc

}
