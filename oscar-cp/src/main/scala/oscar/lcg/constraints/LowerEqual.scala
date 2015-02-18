package oscar.lcg.constraints

import oscar.lcg.variables.LCGIntervalVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/** @author Renaud Hartert ren.hartert@gmail.com */
class LowerEqual(left: LCGIntervalVar, right: LCGIntervalVar) extends LCGConstraint(left.cpStore, "LowerEqual") {

  final override def cdclStore = left.cdclStore

  private[this] val builder = cdclStore.clauseBuilder

  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) Failure
    else {
      left.callWhenBoundsChange(this)
      right.callWhenBoundsChange(this)
      Suspend
    }
  }

  final override def propagate(): CPOutcome = {
    if (updateMax() == Failure) Failure
    else if (updateMin() == Failure) Failure
    else Suspend
  }

  @inline private def updateMax(): CPOutcome = {
    val leftMax = left.max
    val rightMax = right.max
    if (leftMax <= rightMax) Suspend
    else {
      val lit1 = right.lowerEqual(rightMax)
      val lit2 = left.lowerEqual(rightMax)
      builder.clear()
      builder.add(-lit1)
      builder.add(lit2)
      val literals = builder.toArray
      if (!cdclStore.addExplanationClause(literals)) Failure
      else Suspend
    }
  }

  @inline private def updateMin(): CPOutcome = {
    val leftMin = left.min
    val rightMin = right.min
    if (leftMin <= rightMin) Suspend
    else {
      val lit1 = left.greaterEqual(leftMin)
      val lit2 = right.greaterEqual(leftMin)
      builder.clear()
      builder.add(-lit1)
      builder.add(lit2)
      val literals = builder.toArray
      if (!cdclStore.addExplanationClause(literals)) Failure
      else Suspend
    }
  }
}