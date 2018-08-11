package scalene.routing

import scalene.http._

/**
 * An ErrorReason indicates how parsing should proceed once an error has been
 * encountered.  For now the only errors parsers themselves can return
 * corrospond to either a 400 or 404 error.
 *
 * we use an ADT instead of just error code directly since only a small subset
 * actually applies to parser errors and we'd like exhaustive checking in some
 * places, we also may have multiple types with the same code eventually
 */
sealed abstract class ErrorReason(val code : ResponseCode)
object ErrorReason {
  /**
   * Errors with this type indicate a mismatch of a route, causing the current
   * parsing branch to fail but not parsing overall
   */
  case object NotFound extends ErrorReason(ResponseCode.NotFound)

  /**
   * Errors with this type indicate that a valid path was matched, but some
   * other constraint failed, causing all parsing to fail.  No other branches
   * will be attempted
   */
  case object BadRequest extends ErrorReason(ResponseCode.BadRequest)
}

/**
 * A ParseError indicates some kind of error with either the current branch of
 * parsing or the final result
 *
 * This is not an exception because
 * 1.  Exceptions are slow to create
 * 2.  Most of these errors are part of normal routing, so make the message lazily evaluated
 */
case class ParseError(val reason: ErrorReason, lazyMessage: () => String) {

  def message = lazyMessage()

}

object ParseError {

  def badRequest(message: => String): ParseError = ParseError(ErrorReason.BadRequest, () => message)
  def notFound(message: => String): ParseError = ParseError(ErrorReason.NotFound, () => message)
}
