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
  
  private[this] val taskTime = Array.tabulate(nTasks, horizon + 1)((task, time) => {
    val start = starts(task)
    val literal = lcgStore.lcgStore.newVariable(null, start.name + " overlaps " + time, start.name + " not_overlaps " + time)
    val lit1 = start.greaterEqual(time - durations(task) + 1)
    val lit2 = start.lowerEqual(time)
    val clause1 = Array(-lit1, -lit2, literal)
    val clause2 = Array(-literal, lit1)
    val clause3 = Array(-literal, lit2)
    lcgStore.lcgStore.addProblemClause(clause1)
    lcgStore.lcgStore.addProblemClause(clause2)
    lcgStore.lcgStore.addProblemClause(clause3)
    literal
  })

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

    while (time < horizon && sum <= capa) {

      nOverlaps = 0
      nMandatory = 0
      sum = 0

      // Compute 
      var i = 0
      while (i < nTasks && sum <= capa) {
        val lit = taskTime(i)(time)
        val b = lcgStore.lcgStore.isTrue(lit)
        val lst = starts(i).max
        val est = starts(i).min
        val ect = est + durations(i)
        if (lst <= time && time < ect) {
          println(lcgStore.lcgStore.value(lit))
          mandatory(nMandatory) = i
          nMandatory += 1
          sum += demands(i)
        } else if (est <= time && time < ect) {
          overlaps(nOverlaps) = i
          nOverlaps += 1
        }
        i += 1
      }

      // Checker
      if (sum > capa) {
        // Fail
        val literals = new Array[Literal](nMandatory * 2)
        var i = 0
        while (i < nMandatory) {
          val t = mandatory(i)
          val litMin = starts(t).greaterEqual(starts(t).min)
          val litMax = starts(t).lowerEqual(starts(t).max)
          literals(i * 2) = -litMin
          literals(i * 2 + 1) = -litMax
          i += 1
        }
        lcgStore.addExplanation(literals)
      } 
      
      // Explain
      else {  
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