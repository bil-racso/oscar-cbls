package oscar.lcg.constraints

import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.lcg.core.CDCLStore

abstract class LCGConstraint(store: CPStore, name: String) extends Constraint(store, name) {
  
  def cdclStore: CDCLStore 

}