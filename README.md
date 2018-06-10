# Scalene - Fast and Lightweight Scala I/O Framework

Scalene is a multi-threaded, asynchronous, event-based TCP I/O framework
primarily focused on building HTTP servers.

Scalene aims to be the fastest HTTP framework on the JVM while also having a
simple and efficient API.

Take a look at the include [benchmark
example](benchmark/src/main/scala/Main.scala) to get an idea of what things
will look like.

## Current State 

Currently this project is in a very early prototype phase.  I'm still working
on building out the core features and getting the framework into a
production-ready state.  No artifacts are being published yet and test coverage
is light at best.

### Wanna Help?

I welcome any 

### Origins

Scalene is heavily influenced by Tumblr's Colossus framework, of which I was
also the lead developer. Scalene though is not a fork of Colossus, but instead
more like a reimagining with some fairly fundamental design changes.  At least
as of now no code from Colossus is being directly used in Scalene, though much
of the core design is similar.  If at any point I do include original or
modified code from Colossus I'll make it clear which files/classes they are and
include the licensing information in the NOTICES file.  This will be more
likely when I implement more of the Http, redis, and memcached protocols.


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

