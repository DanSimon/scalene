package scalene.routing

import scalene.http._
import scalene.corerouting._

class LazyPathIterator(request: HttpRequest, startIndex: Int = 1) extends Iterator[String] {
  private lazy val pathComponents = request.url.split("/")
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

  def cclone = new LazyPathIterator(request, currentIndex)
}


class RequestContext(val request: HttpRequest, val routingContext: RoutingContext, val pathIterator: LazyPathIterator) extends Clonable[RequestContext] with Iterator[String]{

  def this(request: HttpRequest, routingContext: RoutingContext) = {
    this(request, routingContext, new LazyPathIterator(request))
  }

  //used when branching
  def cclone = new RequestContext(request, routingContext, pathIterator.cclone)

  def hasNext = pathIterator.hasNext

  def next = pathIterator.next


}


