# Scalene - Fast and Lightweight Scala I/O Framework

Scalene is a multi-threaded, asynchronous, reactive TCP I/O framework
primarily focused on building HTTP servers.

Scalene aims to be the fastest HTTP framework on the JVM while also having a
simple and efficient API.  It could also easily serve as a simple abstraction
over NIO for generalized TCP applications, but for now this is not a
first-class goal of the project.

Take a look at the included [benchmark
example](benchmark/src/main/scala/Main.scala) to get an idea of what things
will look like.

## Features

### Powerful Routing DSL

Scalene not only makes it easy to define complex routes, but also extract and
manipulate data from requests in a fluid and typesafe way.

```scala

val route = GET / "foo" / ![Int] / ![String] to {case i :: s :: HNil => s"Got an int $i and a string $s".ok}
//>curl localhost/foo/3/hello
//(200 OK) Got an int 3 and a string hello
//
//>curl localhost/foo/hello/3
//(400 BAD_REQUEST) expected Integer in path segment, got 'hello'

case class Foo(i: Int, s: String)

val route2 = "foo"  subroutes { base =>
  base + GET / ![Int] to {case i :: HNil => 
    foodb.get(id).map{f => s"got $f".ok}
  },
  base + PUT / (![Int].filter{_ > 0} / ![String] >> Foo) to {case foo :: HNil =>
    foodb.create(foo).map{f => s"created $f".ok}
  }
}
//>curl -X PUT localhost/4


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

### Origins

Scalene is heavily influenced by Tumblr's Colossus framework, of which I was
also the creator and lead developer.  Scalene though is not a fork of Colossus, but instead
more like a re-imagining with some fairly fundamental design changes.  

The biggest commonality Scalene shares with Colossus is the general idea of
maximizing concurrency while minimizing parallelism.  Like Colossus, Scalene
aims to keep most I/O operations single-threaded, eliminating the overhead of
context switching, but still asynchronous.  This is not so different from how
other high-performance frameworks like vert.x and rapidoid behave.


Some of the bigger changes are:

* Replaced Akka with built-in specialized actor library.  Colossus only used Akka in a very basic and limited way, so I decided to build a dedicated slimmed-down version containing only what was needed.
* Fully functional and declarative API.  IMO the biggest issue with Colossus was how it handled exposing worker threads to the user.  Scalene solves this by making the entire API declarative and more-or-less completely hides the complexity from the user.
* Powerful routing DSL.  A dramatic improvement from Colossus's pattern-matching DSL, Scalene's HTTP routing combinators allow you to easily define complex routes that parse, filter, and extract typed values from http requests.

## Benchmarks

Scalene is already really fast.  

Once it's in a more complete state and I start publishing artifacts Scalene
will be entered into the techempower benchmarks.  For now, here's the results
of some benchmarks I've run myself:

This benchmark hits the `/plaintext` route.  I used whatever was in master in
the Techempower repo at the time with no modifications except frameworks were
limited to 1 I/O thread.  I used wrk with 75 connections, 2 threads, pipeline
depth of 16.  Benchmarks were run on my 4-core Intel 6700K 4.0Ghz desktop
running Windows 10 with WSL.

framework| requests/second
--- | ---
Scalene 741,068
Rapidoid  595,252
Colossus  357,922

When allowing the number of I/O workers to be default (most are 4, some are 8),
Scalene is basically on-par with Rapidoid, although I have less confidence in
these numbers since such a large percentage of CPU is used by wrk itself and I
don't think the servers anywhere close to being maxed out.

framework| requests/second
--- | ---
Rapidoid | 806,040
Scalene | 805,039
Colossus | 753,206
Netty | 749,442
Finagle | 191,574
http4s | 78,971

