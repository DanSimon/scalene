package scalene.util

trait TimeKeeper {
  def nowMillis : Long

  def apply() = nowMillis

  def refresh(): Long
}

class RealTimeKeeper extends TimeKeeper {
  def nowMillis = System.currentTimeMillis

  def refresh(): Long = nowMillis
}

class RefreshOnDemandTimeKeeper(base: TimeKeeper) extends TimeKeeper {

  private var cached = base()

  def refresh(): Long = {
    cached = base()
    cached
  }

  def nowMillis = cached

}

class TestTimeKeeper(initial: Long) extends TimeKeeper {

  private var current = initial

  def nowMillis = current

  def refresh() = current

  def set(newTime: Long): Unit = {
    current = newTime
  }

}
