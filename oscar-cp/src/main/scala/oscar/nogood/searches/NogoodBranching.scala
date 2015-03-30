package oscar.nogood.searches

import oscar.cp.core.variables.CPIntVar
import oscar.nogood.decisions.Decision
import oscar.nogood.decisions.Assign

abstract class NogoodBranching {
  def reset(): Unit = Unit
  def nextDecision: Decision
}

class StaticNogoodBranching(variables: Array[CPIntVar], valHeuristic: CPIntVar => Int) extends NogoodBranching {
  final override def nextDecision: Decision = {
    val first = variables.find(!_.isBound)
    if (first.isEmpty) null
    else {
      val variable = first.get
      new Assign(variable, valHeuristic(variable))
    }
  }
}