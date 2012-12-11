package oscar.cbls.modeling

import oscar.cbls.invariants.core.computation.IntVar
import oscar.cbls.constraints.lib.global.AllDiff

trait Constraints {
  
  // naming should be the same as in oscar.cp.modeling.Constraints
  
  def alldifferent(variables:Iterable[IntVar]) = new AllDiff(variables) 

}