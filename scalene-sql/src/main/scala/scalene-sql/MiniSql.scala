package scalene.sql

import java.sql.{Connection, DriverManager, PreparedStatement, ResultSet}

import scalene._
import scalene.actor._
import scalikejdbc._

import scala.util.{Failure, Success, Try}

case class MiniSQLConfig(
  url: String,
  username: String,
  password: String,
  numConnections: Int
)

object MiniSQL {


  def client(connectionName: String, host: String, username: String, password: String)(implicit pool: Pool): MiniSQLClient = {
    val config = MiniSQLConfig(host, username, password, 40)
    new MiniSQLClient(connectionName, config)
  }
}

class MiniSQLSession(connection: Connection) {

  val statementCache = new java.util.HashMap[String, PreparedStatement]()

  def prepared(template: String): PreparedStatement = {
    val stmt = {
      val s = statementCache.get(template)
      if (s == null) {
        val statement = connection.prepareStatement(template,ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY )
        statementCache.put(template, statement)
        statement
      } else {
        s
      }
    }
    stmt
  }


}


class MiniSQLClient(connectionName: String, config: MiniSQLConfig)(implicit pool: Pool) {


  trait QueryExecutor {
    def execute(session: MiniSQLSession): Unit
  }

  class SimplePromise[T] {
    var result: Option[Try[T]] = None
  }

  class WrappedQuery[T](runner: MiniSQLSession => T, promise: SimplePromise[T]) extends QueryExecutor {
    def execute(session: MiniSQLSession): Unit = try {
      promise.result = Some(Success(runner(session)))
    } catch {
      case e: Exception => {
        promise.result = Some(Failure(e))
      }
    }
  }

  class MiniSQLConnection(config: MiniSQLConfig)(implicit pool : Pool) extends ExternalBlockingClient[QueryExecutor, Unit] {

    private val connection = DriverManager.getConnection(config.url, config.username, config.password)
    private val session = new MiniSQLSession(connection)

    def sendBlocking(query: QueryExecutor): Try[Unit] = {
      query.execute(session)
      Success(())
    }
  }

  val blockingClients = Array.fill(config.numConnections)(new MiniSQLConnection(config))

  val nextIndex = new java.util.concurrent.atomic.AtomicLong(0)

  def query[T](q: MiniSQLSession => T): Deferred[T] = {
    val sp = new SimplePromise[T]
    val wrapped = new WrappedQuery(q, sp)
    blockingClients(nextIndex.incrementAndGet().toInt % blockingClients.length).send(wrapped).flatMap{_ => sp.result.get match {
      case Success(r) => Deferred.successful(r)
      case Failure(ex) => Deferred.failure(ex)
    }}
  }




}

/*
class TLPreparedStatement(query: String) {
  val id = query.getHashCode

  def bind(f: PreparedStatement => Unit)

}
*/
  
