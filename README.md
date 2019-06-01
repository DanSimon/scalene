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
val route = GET / "foo" / ![Int] / ![String] to {case (i,s) =>
  s"Got an int $i and a string $s".ok
}
```
```
>curl localhost/foo/3/hello
(200 OK) Got an int 3 and a string hello

>curl localhost/foo/hello/3
(400 BAD_REQUEST) expected Integer in path segment, got 'hello'
```
Even with only the basics done, the DSL is powerful and easy to use.

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

Scalene is heavily influenced by Tumblr's Colossus framework, of which I was
also the creator and lead developer for several years.  While Scalene is not a fork of Colossus, it
has much in common with some fairly fundamental design changes.

For the most part Scalene has been written from scratch, but some code is ported from Colossus.  I've included appropriate attribution in those files as well as the NOTICE file.

## Benchmarks

Scalene is already really fast.  

Once it's in a more complete state and I start publishing artifacts Scalene
will be entered into the techempower benchmarks.  For now, here's the results
of some benchmarks I've run myself:

This test hits the `/plaintext` route in the included benchmark example.  I used whatever was in master in
the Techempower repo at the time with no modifications except frameworks were
limited to 1 I/O thread.  I used wrk with 75 connections, 2 threads, pipeline
depth of 16.  Benchmarks were run on my 4-core Intel 6700K 4.0Ghz desktop
running Windows 10 with WSL.

framework| requests/second
--- | ---
Scalene | 661,068
Rapidoid | 595,252
Colossus | 357,922


