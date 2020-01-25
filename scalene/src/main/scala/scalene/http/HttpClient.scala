package scalene.http

import scalene.actor.Pool
import scalene._
import scalene.util._

object HttpClient {
  def futureClient(config: BasicClientConfig)(implicit pool: Pool) = {
    new FutureClient(
      env => client(config, env),
      config
    )
  }

  def client(config: BasicClientConfig, env: AsyncContext) = {
    new BasicClient[HttpRequest, HttpResponse](f => new HttpClientCodec(f, env.time, new Array(0)), config, env)
  }

  def deferredClient(config: BasicClientConfig) = new DeferredClient(
    env => client(config, env),
    config
  )
}
