package scalene

import org.scalatest._
import org.scalamock.scalatest.MockFactory

class DeferredSpec extends FlatSpec with Matchers with BeforeAndAfterAll with MockFactory{

  behavior of "Deferred"

  it should "catch exception in map" in {
    val ctx = mock[AsyncContext]

    //constant
    Deferred.successful("test")
      .map{s => throw new Exception("fail")}
      .resolve(ctx)

    //promise
    val p = new PromiseAsync[String]
    (defer {c => p})
      .map{s => throw new Exception("fail")}
      .resolve(ctx)
    p.succeed("yay")
  }

  it should "catch exception in flatMap" in {
    val ctx = mock[AsyncContext]

    //constant
    Deferred.successful("test")
      .flatMap{s => throw new Exception("fail")}
      .resolve(ctx)

    //promise
    val p = new PromiseAsync[String]
    (defer {c => p})
      .flatMap{s => throw new Exception("fail")}
      .resolve(ctx)
    p.succeed("yay")
  }
}
