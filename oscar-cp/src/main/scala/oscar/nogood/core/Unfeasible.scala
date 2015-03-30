package oscar.nogood.core

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint

class Unfeasible(store: CPStore) extends Constraint(store, "Unfeasible") {
  
  override def setup(l: CPPropagStrength): CPOutcome = CPOutcome.Failure

}