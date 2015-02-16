package oscar.lcg.examples

object SolutionChecker {

  def check(starts: Array[Int], durations: Array[Int], demands: Array[Int], capa: Int, horizon: Int): Boolean = {

    var nOverlaps = 0
    var sum = 0
    var time = 0
    
    while (time < horizon && sum <= capa) {
      nOverlaps = 0
      sum = 0
      var i = 0
      while (i < starts.length && sum <= capa) {
        val lst = starts(i)
        val ect = starts(i) + durations(i)
        if (lst == time && time < ect) sum += demands(i)
        i += 1
      }
      time += 1
    }

    sum <= capa
  }
}