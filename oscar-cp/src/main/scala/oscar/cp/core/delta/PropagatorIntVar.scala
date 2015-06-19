package oscar.cp.core.delta

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.CPStore

class PropagatorIntVar(x: CPIntVar, filter: SnapshotIntVar => CPOutcome, name: String = "PropagatorIntVar") extends Constraint(x.store, name) {
  
  private[this] val _snapshot = x.snapshot
  addSnapshot(x, _snapshot)
  
  @inline final def snapshot = _snapshot
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    x.callPropagateWhenDomainChanges(this)
    CPOutcome.Suspend
  }
  
  override def propagate(): CPOutcome = filter(_snapshot)
}