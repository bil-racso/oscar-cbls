package oscar.lcg.constraints

import oscar.cp.core.CPStore
import oscar.lcg.core.CDCLStore
import oscar.lcg.variables.LCGIntervalVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.lcg.core.Literal
import oscar.lcg.core.LCGSolver
import oscar.lcg.core.True

class DecompTT(starts: Array[LCGIntervalVar], durations: Array[Int], demands: Array[Int], capa: Int, horizon: Int) extends LCGConstraint(starts(0).cpStore, "DecompTT") {

  require(starts.length > 0)

  private[this] val nTasks = starts.length
  private[this] val overlaps = new Array[Int](nTasks)
  private[this] val mandatory = new Array[Int](nTasks)
  private[this] val builder = cdclStore.clauseBuilder

  final override def cdclStore = starts(0).cdclStore

  private[this] val taskTime = Array.tabulate(nTasks, horizon)((task, time) => {
    val start = starts(task)
    // Literals
    val literal = cdclStore.newVariable(null, "[" + start.name + " overlaps " + time + "]", "[" + start.name + " not_overlaps " + time + "]")
    val lit1 = start.greaterEqual(time - durations(task) + 1)
    val lit2 = start.lowerEqual(time)
    // First clause: lit1 and lit2 => literal
    builder.clear()
    builder.add(-lit1)
    builder.add(-lit2)
    builder.add(literal)
    cdclStore.addProblemClause(builder.toArray)
    // Second clause: literal => lit1
    builder.clear()
    builder.add(-literal)
    builder.add(lit1)
    cdclStore.addProblemClause(builder.toArray)
    // Third clause: literal => lit2
    builder.clear()
    builder.add(-literal)
    builder.add(lit2)
    cdclStore.addProblemClause(builder.toArray)
    literal
  })

  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) return Failure
    else {
      var i = 0
      while (i < nTasks) {
        starts(i).callWhenBoundsChange(this)
        i += 1
      }
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {

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
        val b = cdclStore.isTrue(lit)
        val lst = starts(i).max
        val est = starts(i).min
        val ect = est + durations(i)
        if (cdclStore.value(lit) == True) {
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
        builder.clear()
        var i = 0
        while (i < nMandatory) {
          val t = mandatory(i)
          val lit = taskTime(t)(time)
          builder.add(-lit)
          i += 1
        }
        if (!cdclStore.addExplanationClause(builder.toArray)) return Failure
      } // Explain
      else {
        while (nOverlaps > 0) {
          nOverlaps -= 1
          val task = overlaps(nOverlaps)
          val demand = demands(task)
          if (demand + sum > capa) {
            builder.clear()
            var i = 0
            while (i < nMandatory) {
              val t = mandatory(i)
              val lit = taskTime(t)(time)
              builder.add(-lit)
              i += 1
            }
            builder.add(-starts(task).greaterEqual(time - durations(task) + 1))
            builder.add(starts(task).greaterEqual(time + 1))
            if (!cdclStore.addExplanationClause(builder.toArray)) return Failure
          }
        }
      }

      time += 1
    }
    CPOutcome.Suspend
  }
}