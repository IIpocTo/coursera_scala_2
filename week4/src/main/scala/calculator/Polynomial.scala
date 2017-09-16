package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double], c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(delta() match {
      case negative if negative < 0 => Set()
      case zero if zero == 0 => Set((-b() + Math.sqrt(delta())) / (2 * a()))
      case _ => Set(
        (-b() + Math.sqrt(delta())) / (2 * a()),
        (-b() - Math.sqrt(delta())) / (2 * a())
      )
    })
  }
}
