package scalene
package http

import util._
import Method._

import scalene.actor._
import scalene.routing._
import org.scalatest._
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._
import BasicConversions._

class HttpSpec extends AsyncFlatSpec with Matchers with BeforeAndAfterAll{

  behavior of "HttpServer"
  
  val settings = HttpServerSettings(
    serverName = "test",
    maxIdleTime = 10.seconds,
    server = ServerSettings(
      port = 9876,
      addresses = Nil,
      maxConnections = 4096,
      tcpBacklogSize = None,
      numWorkers = Some(1)
    )
  )



  it should "receive a request" taggedAs(org.scalatest.Tag("test")) in {
    implicit val p = new Pool
    val server = Routing.startDetached(settings, Routes(
      GET / "test"  as "foo".ok
    ))
    server.blockUntilReady(1000)
    val client = HttpClient.futureClient(BasicClientConfig.default("localhost", 9876))
    client.send(HttpRequest.get("/test")).map{res =>
      p.shutdown
      assert(res.code == ResponseCode.Ok)
    }
  }

  behavior of "HttpRequestEncoding"

  it should "encode" in {
    val req = HttpRequest.get("/foo")
    val expected = "GET /foo HTTP/1.1\r\nContent-Length: 0\r\n\r\n"
    val encoder = new HttpMessageEncoder[HttpRequest](timeKeeper = new TestTimeKeeper(123))

    encoder.encodeString(req) should equal(expected)
  }

}
