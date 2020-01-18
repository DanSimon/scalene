# Scalene - Fast and Lightweight Scala I/O Framework

Scalene is a multi-threaded, asynchronous, reactive TCP I/O framework
primarily focused on building HTTP servers.

_**Notice** : This project is just getting started!  Lots of core functionality
works but it's missing lots of features and probably super buggy, maybe you can
help with that?_

Probably best to just start with some examples.  The obligatory hello-world:
```scala
import scalene.routing._
import BasicConversions._
object Main extends App {

  val route = GET / "hello" as "Hello, World!".ok

  val settings = Settings.basic("example", 8080)

  Routing.start(settings, route)
}
```
Here's something a little more complex
```scala
//type-safe data extraction from requests
val sumRoute = "sum" / ![Int] / ![Int] to {case (a,b) => 
  (a + b).ok
}

//easily define your own custom data extractors
case class NonZeroInt(value: Int)

def nonZeroInt(name: String): Extractor[NonZeroInt] = ![Int]
  .filter{_ != 0, s"${name} must be nonzero"}
  .map(NonZeroInt.apply)

val Dividend = nonZeroInt("dividend")

//once you define an extractor, you can use it to extract data from any part of the request...

//extract from the path
val quotientRoute = "quotient" / ![Int] / Dividend to {case (a,b) =>
  (a / b.value).ok
}

//or extract from other parts
val otherQuotientRoute = {
  //routes are built as a combination of parsers
  val path        = Path("other-quotient")
  val queryParam  = Parameter("divisor", ![Int])
  val header      = Header("dividend", Dividend) 

  //parser composition is always type-safe
  val incompleteRoute:RouteBuilder[(Int, NonZeroInt)]  =  (path + queryParam + header) 
  incompleteRoute to {(a,b) => (a / b.value).ok}
}


//now build trees of routes
val calcRoutes = GET / "calc" / List(sumRoute, quotientRoute, otherQuotientRoute)

Routing.start(settings, Routes(calcRoutes))
```
```
>curl localhost:8080/calc/sum/5/6
(200 OK) 11

>curl localhost:8080/calc/quotient/5/0
(400 BAD_REQUEST) dividend must be nonzero

>curl localhost:8080/calc/other-quotient?divisor=4
(400 BAD_REQUEST) missing required header 'dividend'
```
It's also easy to open connections to remote systems

```scala
val cache = Memcache.client("memcache-host", 1211)

def slowFib(n) = n match {
  case 1 | 2 => 1
  case n => fib(n - 1) + fib(n - 2)
}

val PositiveInt = ![Int]("num")
  .filter(_ > 0, "num must be positive")

val fibRoute = GET / "fibonacci" / PositiveInt to {n => 
  cache.get(s"fib_$n").map{
    case Some(cached) => cached.ok
    case None => for {
      result <- Async(slowFib(n))
      _      <- memcache.set(s"$fib_n", result.toString)
    } yield result.ok
  }
}

Routing.start(settings, Routes(fibRoute))
```

Hopefully that gives you an idea of what this is all about.
## Features

* **It's really fast!** - Scalene is built from the ground up for low-latency network I/O and follows the design philosophy of _maximizing concurrency while minimizing parallelism_.  When possible, all I/O relative to a single operation is single-threaded.  When writing services you don't have to think about this though, Scalene does all the optimization and thread-management under the hood.
* **Lightweight** - Currently Scalene only has a dependency on Shapeless.  The core library has no dependencies at all.
* **Powerful Declarative Routing DSL** : Composable, functional, self-documenting, and easily configurable and extendable.


There's a lot of other stuff coming soon
* Streaming (mostly done at this point)
* Http2 support
* More web support like static file streaming

## Getting Started
Currently to try it out you'll have to clone the repo and build the jars yourself.  You can easily do this with SBT
```
sbt publishLocal
```
Then in your project just add to your `build.sbt`
```scala
libraryDependencies += "io.scalene" %% "scalene" % "0.1.0-SNAPSHOT" 
```
## Current State 

Currently this project is in a very early prototype phase.  I'm still working
on building out the core features and getting the framework into a
production-ready state.  No artifacts are being published yet and test coverage
is light at best.

### Wanna Help?

I'm excited to work with anyone who's interested in contributing to this
project in any way!

For small changes and bug fixes just open a PR and I'll get to it quickly.
Ideally one such change per request, but it's ok to group them if they're
closely related.  At this moment I'm not requiring test coverage.

Since the framework is still in its early stages and the code is in a high
state of flux, I'd strongly recommend opening an issue detailing any changes
you want to make before beginning your work.  I will have fairly strict
standards about what I will merge in, so it's better we work together early on.

The only real rule I'm enforcing is that no new library dependencies can be added.  

### Origins

Scalene is heavily influenced by Tumblr's [Colossus](https://github.com/tumblr/colossus) framework, of which I was also the creator and lead developer.  Scalene is not a fork of Colossus, but instead more like a re-imagining with some fairly fundamental design changes.  

The vast majority of Scalene was written from scratch, but some code is copied over from Colossus, which I've detailed in the [NOTICE](NOTICE) file and have included appropriate attribution in the relevant files.

## License

Copyright 2019 Dan Simon

Scalene is published under the MIT license.

