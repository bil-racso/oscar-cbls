package oscar.cp.lcg.constraints

import oscar.cp.core.CPStore
import oscar.cp.lcg.core.LCGStore
import oscar.cp.lcg.variables.LCGIntervalVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.lcg.core.Literal

class DecompChecker(starts: Array[LCGIntervalVar], durations: Array[Int], demands: Array[Int], capa: Int, time: Int) extends LCGConstraint(starts(0).lcgStore, starts(0).store, "CheckerLCG") {

  private[this] val nTasks = starts.length
  private[this] val overlaps = new Array[Int](nTasks)
  private[this] var nOverlaps = 0
  
  final override def register(): Unit = {
    var i = 0
    while (i < nTasks) {
      starts(i).callWhenBoundsChange(this)
      i += 1
    }
  }

  final override def explain(): Unit = {
    nOverlaps = 0
    var sum = 0
    var i = 0
    while (i < nTasks && sum <= capa) {
      val lst = starts(i).max
      val ect = starts(i).min + durations(i)
      if (lst <= time && time < ect) {
        overlaps(nOverlaps) = i
        nOverlaps += 1
        sum += demands(i)
      }
      i += 1
    }

    // Start explaining
    if (sum > capa) {
      val literals = new Array[Literal](nOverlaps * 2)
      i = 0
      while (i < nOverlaps) {
        val task = overlaps(i)
        val litMin = starts(task).minGeq(starts(task).min)
        val litMax = starts(task).maxLeq(starts(task).max)
        literals(i * 2) = -litMin
        literals(i * 2 + 1) = -litMax
        i += 1
      }

      lcgStore.addExplanationClause(literals)
    }
  }
}