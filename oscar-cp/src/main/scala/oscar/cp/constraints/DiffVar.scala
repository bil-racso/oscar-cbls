package oscar.cp.constraints

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPIntVar
import oscar.cp.core.CPIntervalVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

final class DiffVar(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "DiffVar") {
  
  // This constraint is implemented as an L1 constraint 
  // to be propagated as early as possible

  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate == Failure) Failure
    else {
      x.callValBindWhenBind(this)
      y.callValBindWhenBind(this)
      Suspend
    }
  }

  @inline final override def valBind(intVar: CPIntervalVar): CPOutcome = {
    if (intVar == x) {
      if (y.removeValue(x.min) == Failure) Failure
      else Success
    } else if (intVar == y) {
      if (x.removeValue(y.min) == Failure) Failure
      else Success
    } else sys.error("unknown variable")
  }

  @inline final override def propagate(): CPOutcome = {
    if (x.isBound) {
      if (y.removeValue(x.min) == Failure) Failure
      else Success
    } else if (y.isBound) {
      if (x.removeValue(y.min) == Failure) Failure
      else Success
    } else Suspend
  }
}