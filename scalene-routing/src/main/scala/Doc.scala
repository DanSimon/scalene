package scalene.routing


case class ParserDoc(pathSegments: List[String], method: String, headers: List[String], queryParams: List[String]) {
  def withPathSegment(segment: String) = copy(pathSegments = this.pathSegments :+ segment)

  def build: String = {
    val params = if (queryParams.isEmpty) "" else "?" + queryParams.mkString("&")
    val path = "/" + pathSegments.mkString("/") + params
    s"$method $path" //TODO: headers
  }

  override def toString = build
}

object ParserDoc {
  val empty = ParserDoc(Nil, "", Nil, Nil)
}

trait DocBuilder[T] {

  def apply(item: T, docs: ParserDoc): ParserDoc

}
