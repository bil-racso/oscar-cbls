package oscar.cp.mem

object SolSelect {

  private val rand = new scala.util.Random(0)

  def sampling2D[Sol](sols: Iterable[(Sol, (Int, Int))]): (Sol, (Int, Int)) = {
    if (sols.size == 1) sols.head
    else {
      val (bound1, bound2) = getMinSols(sols)
      val (delta, c) = lineParameters(bound1, bound2)
      val range = bound2._1 - bound1._1
      val x = rand.nextInt(range + 1) + bound1._1
      val y = x * delta + c
      val sample = (x, y.toInt)

      var closestSol = sols.head
      var minVal = eucl(sample, closestSol._2)
      for (s <- sols.tail) {
        val m = eucl(sample, s._2)
        if (m < minVal) {
          closestSol = s
          minVal = m
        }
      }
      closestSol
    }
  }

  def lineParameters(s1: (Int, Int), s2: (Int, Int)): (Double, Double) = {
    val (x1, y1) = s1
    val (x2, y2) = s2
    val delta = (y1 - y2).toDouble / (x1 - x2)
    val c = y1 - delta * x1
    (delta, c)
  }

  def manh(s1: (Int, Int), s2: (Int, Int)): Double = {
    val (x1, y1) = s1
    val (x2, y2) = s2
    (x1 - x2).abs + (y1 - y2).abs
  }
  
  def eucl(s1: (Int, Int), s2: (Int, Int)): Double = {
    val (x1, y1) = s1
    val (x2, y2) = s2
    math.sqrt(math.pow((x1 - x2), 2).abs + math.pow((y1 - y2), 2).abs)
  }

  def getMinSols[Sol](sols: Iterable[(Sol, (Int, Int))]): ((Int, Int), (Int, Int)) = {
    var min1 = sols.head._2
    var min2 = sols.head._2
    for ((_, sol) <- sols.tail) {
      if (sol._1 < min1._1) {
        min1 = sol
      }
      if (sol._2 < min2._2) {
        min2 = sol
      }
    }
    (min1, min2)
  }
}