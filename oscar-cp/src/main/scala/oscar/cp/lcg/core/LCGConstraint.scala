package oscar.cp.lcg.core

import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

class LCGConstraint(store: CPStore, final val lcgStore: LCGStore) extends Constraint(store, "LCGConstraint") {
  
  idempotent = true
  priorityL2 = CPStore.MaxPriorityL2
  
  final override def setup(l: CPPropagStrength): CPOutcome = propagate()
  
  final override def propagate(): CPOutcome = {
    val failed = lcgStore.propagate()
    if (failed) Failure
    else Suspend
  }
  
  final def addExplanation(literals: Array[Literal]): Unit = {
    lcgStore.addExplanationClause(literals)
    //store.notifyL2(this)
  }
}