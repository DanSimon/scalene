package scalene

trait TimeKeeper {
  def nowMillis : Long

  def apply = nowMillis
}

class RealTimeKeeper extends TimeKeeper {
  def nowMillis = System.currentTimeMillis
}

class RefreshOnDemandTimeKeeper(base: TimeKeeper) extends TimeKeeper {

  private var cached = base()

  def refresh() = {
    cached = base()
    cached
  }

  def nowMillis = cached

}
