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

  def withPool[T](f: Pool => Future[T]): Future[T] = {
    val p = new Pool
    val future = f(p)
    future.map{x => 
      p.shutdown
      p.join
      x
    }
    future
  }

  def withRoutes[T](routes: HttpRoute)(f: FutureClient[HttpRequest, HttpResponse] => Future[T]): Future[T] = withPool{implicit p => 
    val server = Routing.startDetached(settings, routes)
    server.blockUntilReady(1000)
    val client = HttpClient.futureClient(BasicClientConfig.default("localhost", 9876))
    f(client).map{t =>
      server.shutdown()
      server.blockUntilShutdown(1000)
      t
    }
  }

  it should "receive a request"  in  { 
    val routes = Routes(
      GET / "test"  as "foo".ok
    )
    withRoutes(routes){client => 
      client.send(HttpRequest.get("/test")).map{res =>
        assert(res.code == ResponseCode.Ok)
      }
    }
  }

  it should "send a stream response" in {
    val routes = Routes(
      GET / "test" to {_ => scalene.stream.Stream.fromIter(List("a", "b", "c").toIterator).ok}
    )
    withRoutes(routes){client =>
      client.send(HttpRequest.get("/test")).map{res =>
        assert(res.code == ResponseCode.Ok)
      }
    }
  }

  it should "receive a streamed body" taggedAs(org.scalatest.Tag("test")) in {
    val routes = Routes(
      GET / "test" to {_ => scalene.stream.Stream.fromIter(List("a", "b", "c").toIterator).ok}
    )
    withPool{implicit p => 
      val server = Routing.startDetached(settings, routes)
      server.blockUntilReady(1000)
      val client = HttpClient.deferredClient(BasicClientConfig.default("localhost", 9876))
      val executor = new DeferredExecutor
      executor(
        client
          .send(HttpRequest.get("/test"))
          .flatMap{res =>
            assert(res.code == ResponseCode.Ok)
            res.body.data.collect().map{buf => 
              println("here")
              assert(buf.readString == "abc")
            }
          }
      )
    }
  }

  it should "handle exception thrown in server" in {
    val route = Routes(GET / "test" to {_ => throw new Exception("dead")})
    withRoutes(route){client =>
      client
        .send(HttpRequest.get("/test"))
        .map{res => assert(res.code == ResponseCode.Error)}
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
