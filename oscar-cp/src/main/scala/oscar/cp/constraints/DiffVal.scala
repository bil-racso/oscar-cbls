package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

final class DiffVal(x: CPIntVar, v: Int) extends Constraint(x.store, "DiffVal") {
  
  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (x.removeValue(v) == Failure) Failure
    else Success
  }
}