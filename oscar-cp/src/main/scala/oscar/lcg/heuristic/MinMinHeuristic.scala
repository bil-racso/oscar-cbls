package oscar.lcg.heuristic

import oscar.lcg.variables.LCGIntervalVar

/** @author Renaud Hartert ren.hartert@gmail.com */
class MinMinHeuristic(variables: Array[LCGIntervalVar]) extends Heuristic {
  
  require(variables.length > 0, "no variable")
  
  private[this] val x = variables.toIndexedSeq
  private[this] val cdclStore = variables(0).cdclStore
  
  final override def decision: Function0[Unit] = {
    x.foreach(_.updateAndNotify())
    var i = 0
    var minId = -1
    var min = Int.MaxValue
    while (i < variables.length) {
      val variable = variables(i)
      if (!variable.isAssigned && variable.min < min) {
        minId = i
        min = variable.size
      }
      i += 1
    }
    if (minId == -1) null
    else {
      val x = variables(minId)
      val value = x.min
      val literal1 = x.lowerEqual(value)
      val literal2 = x.greaterEqual(value)
      () => {
        //println("decision  : assign " + x.name + " to " + value)
        cdclStore.enqueue(literal1, null)
        cdclStore.enqueue(literal2, null)
      }
    }
  }
}