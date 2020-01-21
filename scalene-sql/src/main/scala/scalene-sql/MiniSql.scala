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


  def client(connectionName: String, host: String, username: String, password: String, numWorkers: Option[Int] = None)(implicit pool: Pool): MiniSQLClient = {
    val workers = numWorkers.getOrElse(Runtime.getRuntime.availableProcessors())
    val config = MiniSQLConfig(host, username, password, workers)
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

  private trait QueryExecutor {
    def execute(session: MiniSQLSession): Unit
  }

  private class WrappedQuery[T](runner: MiniSQLSession => T, promise: Try[T] => Unit) extends QueryExecutor {
    def execute(session: MiniSQLSession): Unit = try {
      promise(Success(runner(session)))
    } catch {
      case e: Exception => {
        promise(Failure(e))
      }
    }
  }

  private def miniSQLConnection(config: MiniSQLConfig) = {
    val connection = DriverManager.getConnection(config.url, config.username, config.password)
    val session = new MiniSQLSession(connection)
    implicit val d = pool.createDispatcher("minisql-client")
    SimpleReceiver[QueryExecutor](_.execute(session))
  }

  private val blockingClients = Array.fill(config.numConnections)(miniSQLConnection(config))

  private val nextIndex = new java.util.concurrent.atomic.AtomicLong(0)

  def query[T](q: MiniSQLSession => T): Deferred[T] = defer { context =>
    val (promise, async) = context.threadSafePromise[T]()
    val wrapped = new WrappedQuery(q, promise)
    blockingClients(nextIndex.incrementAndGet().toInt % blockingClients.length).send(wrapped)
    async
  }

}

