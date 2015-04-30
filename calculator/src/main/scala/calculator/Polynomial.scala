package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    def solution(delta2: Double): Set[Double] = {
        if (delta2 < 0) Set()
        else Set((0 - b() + math.sqrt(delta2)) / (2 * a()),
            (0 - b() - math.sqrt(delta2)) / (2 * a()))
    } 
    
    Signal(solution(delta()))
  }
}
