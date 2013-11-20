package oscar.cbls.invariants.tests

import oscar.cbls.invariants.core.propagation.Checker

class InvariantChecker extends Checker {
  var firstCheck = true
  var invariantChecked = false

  def check(verity: Boolean, traceOption: Option[String] = None) = {
    if (traceOption.isDefined && !verity)
      println("Counter-example found: " + traceOption.getOrElse(""))
    if (firstCheck) {
      firstCheck = false
      invariantChecked = verity
    }
    else {
      invariantChecked = invariantChecked && verity
    }
    println(invariantChecked)
  }

  def isChecked() = {
    invariantChecked
  }
}
