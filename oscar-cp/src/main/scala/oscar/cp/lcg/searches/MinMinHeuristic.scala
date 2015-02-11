package oscar.cp.lcg.searches

import oscar.cp.lcg.variables.LCGIntervalVar

class MinMinHeuristic(variables: Array[LCGIntervalVar]) extends Heuristic {
  
  require(variables.length > 0, "no variable")
  
  private[this] val x = variables.toIndexedSeq
  private[this] val lcgStore = variables(0).lcgStore
  
  final override def decision: Function0[Unit] = {
    x.foreach(_.updateAndNotify())
    var i = 0
    var minId = -1
    var min = Int.MaxValue
    while (i < variables.length) {
      val variable = variables(i)
      if (!variable.isAssigned && variable.min < min) {
        minId = i
        min = variable.min
      }
      i += 1
    }
    if (minId == -1) null
    else {
      val x = variables(minId)
      val value = x.min
      val literal = x.lowerEqual(value)
      () => {
        println("decision  : assign " + x.name + " to " + value)
        lcgStore.enqueue(literal, null)
      }
    }
  }
}