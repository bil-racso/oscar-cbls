package oscar.cp.lcg.searches

import oscar.cp.lcg.variables.LCGIntervalVar

class MinMinHeuristic(variables: Array[LCGIntervalVar]) extends Heuristic {
  
  require(variables.length > 0, "no variable")
  
  private[this] val lcgStore = variables(0).lcgStore
  
  final override def decision: Function0[Unit] = {
    val variable = variables.find(!_.isAssigned)
    if (variable.isEmpty) null
    else {
      val x = variable.get
      val value = x.min
      val literal = x.lowerEqual(value)
      () => {
        println("decision: assign " + x.name + " to " + value)
        lcgStore.enqueue(literal, null)
      }
    }
  }

}