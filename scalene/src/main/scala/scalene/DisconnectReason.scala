package scalene

sealed abstract class DisconnectReason(
  val name: String, 
  val description: String
)

object DisconnectReason {

  case object LocalClosed extends DisconnectReason ("localclosed", "Closed by local host")

  case object RemoteClosed extends DisconnectReason ("remoteclosed", "Closed by remote host")

  case object Shutdown extends DisconnectReason ("terminated", "Server is shutting down")

  case object TimeOut extends DisconnectReason ( "timeout", "Timed out")

  case class Error(error: Throwable) extends DisconnectReason ("error", s"Error: $error")

  case class ClientConnectFailed(error: Throwable) extends DisconnectReason ("connectfailed", "Failed to Connect")
}
