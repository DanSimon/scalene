# Scalene - Fast and Lightweight Scala I/O Framework

Scalene is a multi-threaded, asynchronous, event-based TCP I/O framework
primarily focused on building HTTP servers.

Scalene aims to be the fastest HTTP framework on the JVM while also having a
simple and efficient API.

Take a look at the included [benchmark
example](benchmark/src/main/scala/Main.scala) to get an idea of what things
will look like.

## Current State 

Currently this project is in a very early prototype phase.  I'm still working
on building out the core features and getting the framework into a
production-ready state.  No artifacts are being published yet and test coverage
is light at best.

Roughly I see this project having 3 main stages

1.  0.0.x phase (aka current prototype phase) - core development, things changing all the time, missing fundamental features
2.  0.x phase - still lots of development, frequent breaking changes, but the basics should be built and stable enough to use in pet projects, non-production environments
3.  x.0 phase - production-ready, proper semantic versioning and releases

I currently have no roadmap for when it'll hit 0.x and what functionality it will have.

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

## Origins

Scalene is heavily influenced by Tumblr's Colossus framework, of which I was
also the lead developer. Scalene though is not a fork of Colossus, but instead
more like a re-imagining with some fairly fundamental design changes.  At least
as of now no code from Colossus is being directly used in Scalene, though much
of the core design is similar.  If at any point I do include original or
modified code from Colossus I'll make it clear which files/classes they are and
include the licensing information in the NOTICES file.  This will be more
likely when I implement more of the Http, redis, and memcached protocols.

### Why Scalene?

Why am I building a new framework and not simply doing a big overhaul of Colossus?  

* Colossus already went through a few big overhauls which has left a fair
  amount of legacy design decisions.  In particular Colossus was built for some
use cases which I now think are not very realistic.  A lot of work went into
making it easy to implement custom binary protocols, but in all honestly it's
not very often that someone actually wants to implement a proprietary protocol.
Scalene will aim to support basically just http and http/2 for server
protocols, possibly gRPC and websocket.  This doesn't mean it will be
particularly difficult to develop other protocols, but Scalene will not have a
"library within a framework" for building protocols.

*   Colossus has had a relatively stable API for a while now.  Given the
    differences I have made/planned with Scalene so far, it would be a highly
disruptive set of changes to implement in Colossus; it really wouldn't even
feel like the same framework.

* Colossus is still owned and controlled by Tumblr, but seems to be inactive and unmaintained (I left Tumblr in mid 2017).


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

Scalene 1,041,538
Rapidoid  595,252
Colossus  357,922
Finagle 191,574
http4s

