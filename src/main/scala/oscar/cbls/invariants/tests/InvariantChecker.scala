package oscar.cbls.invariants.tests

import oscar.cbls.invariants.core.propagation.Checker

class InvariantChecker extends Checker {
  var invariantChecked = true

  def check(verity: Boolean, traceOption: Option[String] = None) = {
    if (traceOption.isDefined && !verity)
      println("Counter-example found: " + traceOption.getOrElse(""))
    invariantChecked = invariantChecked && verity
  }

  def isChecked() = {
    invariantChecked
  }
}
