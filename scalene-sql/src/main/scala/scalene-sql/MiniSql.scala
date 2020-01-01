package scalene.sql

import scalene._
import scalene.actor._
import scalikejdbc._

import scala.util.{Failure, Success, Try}

object MiniSQL {

  def client(connectionName: String, host: String, username: String, password: String)(implicit pool: Pool): MiniSQLClient = {
    val settings = ConnectionPoolSettings(
      initialSize = 5,
      maxSize = 20,
      connectionTimeoutMillis = 3000L
      )
    ConnectionPool.add(connectionName, host, username, password, settings)
    new MiniSQLClient(connectionName, 16)
  }
}

class MiniSQLClient(connectionName: String, numConnections: Int)(implicit pool: Pool) {


  trait QueryExecutor {
    def execute(session: DBSession): Unit
  }

  class SimplePromise[T] {
    var result: Option[Try[T]] = None
  }

  class WrappedQuery[T](runner: DBSession => T, promise: SimplePromise[T]) extends QueryExecutor {
    def execute(session: DBSession): Unit = try {
      promise.result = Some(Success(runner(session)))
    } catch {
      case e: Exception => {
        promise.result = Some(Failure(e))
      }
    }
  }

  def sendFunc: QueryExecutor => Try[Unit] = {
    //TODO: need to handle closing connections!
    val connection = ConnectionPool.borrow(connectionName)
    ex => {
      val db = DB(connection)
      db.autoClose(false)
      db.localTx{ implicit session =>
        ex.execute(session)
      }
      Success(())
    }
  }
      
  val blockingClients = Array.fill(numConnections)(new ExternalBlockingClient(sendFunc))

  val nextIndex = new java.util.concurrent.atomic.AtomicLong(0)

  def query[T](q: DBSession => T): Deferred[T] = {
    val sp = new SimplePromise[T]
    val wrapped = new WrappedQuery(q, sp)
    blockingClients(nextIndex.incrementAndGet().toInt % blockingClients.length).send(wrapped).flatMap{_ => sp.result.get match {
      case Success(r) => Deferred.successful(r)
      case Failure(ex) => Deferred.failure(ex)
    }}
  }




}
