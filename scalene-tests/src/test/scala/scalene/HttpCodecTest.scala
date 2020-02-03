package scalene
package http

import util._

import org.scalatest._

class HttpCodecSpec extends FlatSpec with Matchers with BeforeAndAfterAll{

  behavior of "HttpServerCodec" 

  it should "decode a simple request" in {
    val req = "GET /foo/bar HTTP/1.1\r\n\r\n"
    var decoded: Option[HttpRequest] = None
    val codec = new HttpServerCodec(x => decoded = Some(x), new RealTimeKeeper, Nil.toArray)

    codec.decode(ReadBuffer(req))

    val r = decoded.get
    r.method should equal(Method.Get)
    r.url should equal("/foo/bar")
  }

  it should "decode a request with headers" in {
    val req = "GET /foo/bar HTTP/1.1\r\nheader1:value1\r\nheader2:value2\r\n\r\n"
    var decoded: Option[HttpRequest] = None
    val codec = new HttpServerCodec(x => decoded = Some(x), new RealTimeKeeper, Nil.toArray)

    codec.decode(ReadBuffer(req))

    val r = decoded.get
    r.method should equal(Method.Get)
    r.url should equal("/foo/bar")
    r.headers.size should equal(2)
    r.headers.firstValue("header1") should equal(Some("value1"))
    r.headers.firstValue("header2") should equal(Some("value2"))
  }

  it should "decode several pipelined requests" in {
    def req(i: Int) = s"GET /foo/$i HTTP/1.1\r\nheader1:$i\r\nheader2:value2\r\n\r\n"

    val data = ReadBuffer((0 to 4).map{i => req(i)}.mkString)
    val decoded = collection.mutable.ArrayBuffer[HttpRequest]()
    val codec = new HttpServerCodec(x => decoded.append(x), new RealTimeKeeper, Nil.toArray)

    codec.decode(data)

    decoded.size should equal(5)
    decoded.zipWithIndex.foreach{ case (r, i) =>
      r.url should equal(s"/foo/$i")
      r.headers.firstValue("header1") should equal(Some(i.toString))
    }
  }

  it should "decode a request in several chunks" in {
    val req = "GET /foo/bar HTTP/1.1\r\nheader1:value1\r\nheader2:value2\r\n\r\n"

    (1 until req.length - 1).foreach{i => 
      val p1 = req.take(i)
      val p2 = req.drop(i)
      var decoded: Option[HttpRequest] = None
      val codec = new HttpServerCodec(x => decoded = Some(x), new RealTimeKeeper, Nil.toArray)
      codec.decode(ReadBuffer(p1))
      decoded should equal(None)

      codec.decode(ReadBuffer(p2))
      val r = decoded.get
      r.method should equal(Method.Get)
      r.url should equal("/foo/bar")
      r.headers.size should equal(2)
      r.headers.firstValue("header1") should equal(Some("value1"))
      r.headers.firstValue("header2") should equal(Some("value2"))
    }

  }

}

  

