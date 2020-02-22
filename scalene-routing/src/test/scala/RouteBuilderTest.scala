package scalene.routing

import scalene._
import scalene.http._
import scalene.corerouting.AsResponse
import org.scalatest._
import org.scalamock.scalatest.MockFactory
import BasicConversions._

class RouteBuilderTest extends WordSpec with MustMatchers with MockFactory{

  def fakeRHC = new RequestHandlerContext {
    def time = ???
    def closeConnection = ()
  }
  val routingContext = RoutingContext(new AttachmentManager(fakeRHC), fakeRHC)

  "RouteBuilderExecution" must {
    "execute filter once" in {
      var num = 0
      val filter: Filter[RequestContext, Unit] = Filter(r => {
        Deferred.successful(num += 1)
      })

      val builder = RouteBuilder.one(CellFilter(filter))
      val route = builder to {_ => "ok".ok}

      route(new RequestContext(HttpRequest.get("/foo"), routingContext))
        .right.get
        .resolve(mock[AsyncContext])

      num must equal(1)
    }

    "execute filter in subroute once" in {
      var num = 0
      val filter: Filter[RequestContext, Unit] = Filter(r => {
        Deferred.successful(num += 1)
      })

      val builder = RouteBuilder.one(CellFilter(filter))
      val route = filter subroutes(
        _ + GET to {_ => "ok".ok}
      )

      route(new RequestContext(HttpRequest.get("/foo"), routingContext))
        .right.get
        .resolve(mock[AsyncContext])

      num must equal(1)
    }

  }

}

