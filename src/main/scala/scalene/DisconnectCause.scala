package scalene

sealed trait DisconnectReason

object DisconnectReason {

  case object LocalClosed extends DisconnectReason {
    def tagString = "disconnect"
    def logString = "Closed by local host"
  }

  case object RemoteClosed extends DisconnectReason {
    def tagString = "closed"
    def logString = "Closed by remote host"
  }

  case object SystemShutdown extends DisconnectReason {
    def tagString = "terminated"
    def logString = "IO System is shutting down"
  }

  case object TimedOut extends DisconnectReason {
    def tagString = "timedout"
    def logString = "Timed out"
  }

  case class Error(error: Throwable) extends DisconnectReason {
    def tagString = "error"
    def logString = s"Error: $error"
  }

  case class ClientConnectFailed(error: Throwable) extends DisconnectReason {
    def tagString = "connectfailed"
    def logString = "Failed to Connect"
  }
}
