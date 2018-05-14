#Ideas

## Declarative Approach to Request Handlers

Aka "Let's kill the initializer"

One of the biggest weirdness things in Colossus was the need to separate logic
between the initializer and the request handler.  In particular, outgoing
client connections were supposed to be created in the initializer and passed to
the request handler in a nested block.  This was how you would open one client
connection per thread instead of one per request handler.

But we shouldn't need to do this.  A request handler should just be able to
define a client connection and let the framework handle the actual
threading/pooling logic.  So we'd just have something like :

```scala
class RequestHandler(implicit context: Context) {

  val memcached = MemcachedClient(clientSettings)

}
```
And behind the scenes the context would allow some kind of pool to choose the
correct actual connection to use.  DONT FORGET TESTING

This primarily the only reason we needed to expose the initializer to the user,
so taking this approach would dramatically clean everything up.

This still doesn't quite get us to a singleton request handler, since it's
assumed the context is a per-connection context.  Otherwise we'd have to create
a per-request context or at the very least include a context along with the
request.

## Going through Worker

Let's figure out what we need and what we don't.

### Move all server logic out of worker

First, we cannot make workers just for server connections because they have to
be able to handle client connections as well.

Can we make connection handlers actors?  Maybe not the handlers themselves, but
they could probably create an internal actor.  We can't make the handler itself
an actor because right now an error causes it to get recreated

Instead, basically all 
