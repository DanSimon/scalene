package example

import shapeless.{HList, ::, HNil}
import router._
import microactor._
import scalene._
import scalene.http.{Method => HttpMethod, _}
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import java.util.concurrent.atomic.AtomicLong

case class Foo(i: Int, s: String)

object Main extends App {
  //Benchmark.test()


  Server2.startServer()
}

object Server2 {

  val settings = HttpServerSettings(
    serverName = "router",
    maxIdleTime = 10.seconds,
    server = ServerSettings(
      port = 9001,
      addresses = Nil,
      maxConnections = 4096,
      tcpBacklogSize = None,
      numWorkers = Some(1),
    )
  )

  def start(routes: BuiltRoute[RequestContext, HttpResponse]) = {
    implicit val p = new Pool
    HttpServer.start(settings, implicit context => new RequestHandler[HttpRequest, HttpResponse] {
    
      def onInitialize(context: RequestHandlerContext){}

      def handleRequest(request: HttpRequest): Async[HttpResponse] = routes(new RequestContext(request)) match {
        case Right(f) => f.resolve(context)
        case Left(reason) => Async.successful(HttpResponse(reason.reason.code, Body.plain(reason.message)))
      }

      def handleError(request: Option[HttpRequest], error: Throwable) = HttpResponse(ResponseCode.Error, Body.plain(error.toString))

    })
    p.join

  }

  val myFilter: Filter[RequestContext, Int] = RequestFilter(context => Deferred.successful(3))

  implicit class Futurize[T <: Any](val thing: T) extends AnyVal {
    def future = Deferred.successful(thing)
  }

  import R._
  val routes2 = GET + Url("/plaintext") to {_ => 
    Body.plain("Hello, World!").ok.future
  }

  val routes = Path(RT / "asdf") subroutes (
    _ + myFilter + Path(RT / "foo" / ![Int] / ![String]) to {case i :: a :: b :: HNil => Body.plain("got it").ok.future},
    _ + Path(RT / "bar") subroutes ( 
      _ + Path(RT / "a") to {_ => Body.plain("ok a").ok.future},
      _ + Path(RT / "b") to {_ => Body.plain("ok b").ok.future}
    )
  )

  import R._

  val test = GET + Url("/test") + Parameter("foo", "bar") +  Parameter("bar", ![String])

  def startServer() {
    start(routes2.toRoute)
  }

}


