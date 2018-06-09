# Scalene - Fast and Lightweight Scala I/O Framework

Scalene is a multi-threaded, asynchronous, event-based TCP I/O framework
primarily focused on building HTTP servers.

Scalene aims to be the fastest HTTP framework on the JVM while also having a
simple and efficient API.


## Current State of Things

Currently this project is in a very early prototype phase.  I'm still working on building
out the core features and getting the framework into a production-ready state.
No artifacts are being published yet and test coverage is light at
best.

### Wanna Help?

I welcome any 

## Origins

Scalene is heavily influenced by Tumblr's Colossus framework, of which I was
also the lead developer. Scalene though is not a fork Colossus, but instead
more like a reimagining with some fairly fundamental design changes.  At least
as of now, no code from Colossus is being directly used in Scalene, though much
of the core design is similar.  If at any point I do include original or
modified code from Colossus I'll make it clear which files/classes they are and
include the licensing information in the NOTICES file.  This will be more
likely when I implement more of the Http, redis, and memcached protocols.

A few ways in which Scalene differs from Colossus:

1.  Instead of Akka, Scalene is built on my own micro-actor library.  Colossus
    only uses Akka in a very simple way, more like a fancy thread-pool manager,
    so I decided to build a slimmed down and optmized actor library that does
    exactly what Scalene needs and nothing more.

2.  Scalene's API (when complete) will have a more declarative feel and should be much easier to reason about.  In particular the kind of weird function nesting you had to do in Colossus to build a server will be totally gone.  Also a ton of effort went into making certain interfaces work with both 

2.  Scalene is entirely built on a concept called elastic backpressure.  Colossus i

3.  A whole bunch of small but significant optimizations in the core mechanics
    around how buffers and data are managed.  In particular there's a few changes that seem to take better advantage of CPU caches and I've 

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

In another benchmark I ran all the frameworks in a Ubuntu VM
