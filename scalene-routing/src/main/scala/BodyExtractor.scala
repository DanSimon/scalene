package scalene.routing

import scalene.http._
import scala.annotation.implicitNotFound
import scalene.corerouting._
import scalene.ReadBuffer

object Body extends Filter[RequestContext, ReadBuffer] {

  def apply(r: RequestContext): Deferred[ReadBuffer] = r.request.body.data.collect()

  def raw = new Parser[RequestContext, Body] {
    def parse(r: RequestContext): Result[Body] = Right(r.request.body)
  }
}


