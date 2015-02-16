package oscar.cp.lcg.constraints

import oscar.cp.core.CPStore
import oscar.cp.lcg.core.LCGStore
import oscar.cp.lcg.variables.LCGIntervalVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.lcg.core.Literal
import oscar.cp.lcg.core.LCGSolver

class DecompChecker(lcgSolver: LCGSolver, starts: Array[LCGIntervalVar], durations: Array[Int], demands: Array[Int], capa: Int, horizon: Int) extends LCGConstraint(lcgSolver, starts(0).store, "CheckerLCG") {

  private[this] val nTasks = starts.length
  private[this] val overlaps = new Array[Int](nTasks)

  final override def register(): Unit = {
    var i = 0
    while (i < nTasks) {
      starts(i).callWhenBoundsChange(this)
      i += 1
    }
  }

  final override def explain(): CPOutcome = {

    var nOverlaps = 0
    var sum = 0
    var time = 0
    
    while (time < horizon && sum <= capa) {
      nOverlaps = 0
      sum = 0
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
      time += 1
    }

    // Start explaining
    if (sum > capa) {
      val literals = new Array[Literal](nOverlaps * 2)
      var i = 0
      while (i < nOverlaps) {
        val task = overlaps(i)
        val litMin = starts(task).greaterEqual(starts(task).min)
        val litMax = starts(task).lowerEqual(starts(task).max)
        literals(i * 2) = -litMin
        literals(i * 2 + 1) = -litMax
        i += 1
      }

      lcgStore.addExplanation(literals)
    }
    Suspend
  }
}