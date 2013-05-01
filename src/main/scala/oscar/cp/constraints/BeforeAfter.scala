package oscar.cp.constraints

import oscar.cp.scheduling.Activity
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPPropagStrength

class BeforeAfter(private val act: Activity, val before: Int, val after: Int) extends Constraint(act.scheduler, "BeforeAfter constraint") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (updateMin(act.end, 0) == Failure) Failure
    else if (updateMax(act.start, 0) == Failure) Failure
    else {
      act.end.callUpdateMinWhenMinChanges(this)
      act.start.callUpdateMaxWhenMaxChanges(this)
      Suspend
    }
  }

  override def updateMin(end: CPVarInt, v: Int): CPOutcome = {
    if (act.end.min < before) Suspend
    else if (act.start.updateMin(after) == Failure) Failure
    else Success
  }
  
  override def updateMax(start: CPVarInt, v: Int): CPOutcome = {
    if (act.start.max > 60) Suspend
    else if (act.end.updateMax(before) == Failure) Failure
    else Success
  }
}