package oscar.cp.lcg.constraints

import oscar.cp.core.CPStore
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint

class DecompCheckerCP(starts: Array[CPIntVar], durations: Array[Int], demands: Array[Int], capa: Int, horizon: Int) extends Constraint(starts(0).store, "CheckerCP") {

  private[this] val nTasks = starts.length
  private[this] val overlaps = new Array[Int](nTasks)

  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate == Failure) Failure
    else {
      var i = 0
      while (i < nTasks) {
        starts(i).callPropagateWhenBoundsChange(this)
        i += 1
      }
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {

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
    if (sum > capa) Failure
    else Suspend
  }
}