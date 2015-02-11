package oscar.cp.lcg.constraints

import oscar.cp.core.CPStore
import oscar.cp.lcg.core.LCGStore
import oscar.cp.lcg.variables.LCGIntervalVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.lcg.core.Literal
import oscar.cp.lcg.core.LCGSolver

class DecompTT(lcgSolver: LCGSolver, starts: Array[LCGIntervalVar], durations: Array[Int], demands: Array[Int], capa: Int, horizon: Int) extends LCGConstraint(lcgSolver, starts(0).store, "CheckerLCG") {

  private[this] val nTasks = starts.length
  private[this] val overlaps = new Array[Int](nTasks)
  private[this] val mandatory = new Array[Int](nTasks)

  final override def register(): Unit = {
    var i = 0
    while (i < nTasks) {
      starts(i).callWhenBoundsChange(this)
      i += 1
    }
  }

  final override def explain(): Unit = {

    var nOverlaps = 0
    var nMandatory = 0
    var sum = 0
    var time = 0

    while (time < horizon) {

      nOverlaps = 0
      nMandatory = 0
      sum = 0

      // Compute 
      var i = 0
      while (i < nTasks && sum <= capa) {
        val lst = starts(i).max
        val est = starts(i).min
        val ect = est + durations(i)
        if (lst <= time && time < ect) {
          mandatory(nMandatory) = i
          nMandatory += 1
          sum += demands(i)
        } else if (est <= time && time < ect) {
          overlaps(nOverlaps) = i
          nOverlaps += 1
        }
        i += 1
      }

      if (sum > capa) {
        println("don't want to handle that for the moment")
        // Fail
        val literals = new Array[Literal](nMandatory * 2 + 1)
        var i = 0
        while (i < nMandatory) {
          val t = mandatory(i)
          val litMin = starts(t).greaterEqual(starts(t).min)
          val litMax = starts(t).lowerEqual(starts(t).max)
          literals(i * 2) = -litMin
          literals(i * 2 + 1) = -litMax
          i += 1
        }
        literals(literals.length - 1) = lcgStore.lcgStore.falseLit
        lcgStore.addExplanation(literals)
      } else {  
        // Explain
        while (nOverlaps > 0) {
          nOverlaps -= 1
          val task = overlaps(nOverlaps)
          val demand = demands(task)
          if (demand + sum > capa) {
            val literals = new Array[Literal](nMandatory * 2 + 1)
            var i = 0
            while (i < nMandatory) {
              val t = mandatory(i)
              val litMin = starts(t).greaterEqual(starts(t).min)
              val litMax = starts(t).lowerEqual(starts(t).max)
              literals(i * 2) = -litMin
              literals(i * 2 + 1) = -litMax
              i += 1
            }
            literals(literals.length - 1) = starts(task).greaterEqual(time + 1)
            lcgStore.addExplanation(literals)
          }
        }
      }

      time += 1
    }
  }
}