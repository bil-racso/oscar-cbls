package oscar.cp.core.delta

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPStore

class PropagatorIntVar(x: CPIntVar, id: Int, filter: DeltaIntVar => CPOutcome, name: String = "PropagatorIntVar") extends Constraint(x.store, name) {
  
  private[this] val _delta = x.delta(id, this)
  
  final def priority: Int = this.priorityL2
  final def priority_=(level: Int): Unit = this.priorityL2 = level
  
  @inline final def snapshot = _delta
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    x.callPropagateWhenDomainChanges(this)
    CPOutcome.Suspend
  }
  
  override def propagate(): CPOutcome = filter(_delta)
}