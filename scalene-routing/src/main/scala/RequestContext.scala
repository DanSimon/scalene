package scalene.routing

import scalene.http._
import scalene.corerouting._

class LazyPathIterator(getUrl: () => String, startIndex: Int = 1) extends Iterator[String] {
  private lazy val pathComponents = getUrl().split("/")
  private var currentIndex = startIndex

  def hasNext = currentIndex < pathComponents.size

  def next = {
    val res = pathComponents(currentIndex)
    currentIndex += 1
    res
  }

  def advance(num: Int): Unit = {
    currentIndex += num
  }

  def cclone = new LazyPathIterator(getUrl, currentIndex)
}


class RequestContext(val request: HttpRequest, val pathIterator: LazyPathIterator) extends Clonable[RequestContext] with Iterator[String]{

  def this(request: HttpRequest) = {
    this(request, new LazyPathIterator(() => request.url))
  }

  //used when branching
  def cclone = new RequestContext(request, pathIterator.cclone)

  def hasNext = pathIterator.hasNext

  def next = pathIterator.next

}
