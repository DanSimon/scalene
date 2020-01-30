package scalene.routing

import scalene._

class AttachmentManager(rctx: RequestHandlerContext) {
  private lazy val attachments = new java.util.HashMap[Int, Any]()

  def set[T](provider: AttachmentProvider[T], attachment: T): Unit = {
    attachments.put(provider.hashCode, attachment)
  }

  def get[T](provider: AttachmentProvider[T]): Option[T] = {
    Option(attachments.get(provider)).map{_.asInstanceOf[T]}
  }

  def getOrCreate[T](provider: AttachmentProvider[T]): T = {
    if (attachments.containsKey(provider.hashCode)) {
      attachments.get(provider.hashCode).asInstanceOf[T]
    } else {
      val attachment = provider.provide(rctx)
      attachments.put(provider.hashCode, attachment)
      attachment
    }
  }

}


trait AttachmentProvider[T] {
  def provide(ctx: RequestHandlerContext): T
}
