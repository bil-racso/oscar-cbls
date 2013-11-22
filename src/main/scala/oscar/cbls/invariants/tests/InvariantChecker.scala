package oscar.cbls.invariants.tests

import oscar.cbls.invariants.core.propagation.Checker

class InvariantChecker(verbose: Boolean = false) extends Checker {
  var firstCheck = true
  var invariantChecked = false

  def check(verity: Boolean, traceOption: Option[String]) = {
    if (traceOption.isDefined) {
      val trace = traceOption.get
      if (!verity) println("Counter-example found: " + trace)
      else if (verbose) println(trace + " is " + invariantChecked)
    }

    if (firstCheck) {
      firstCheck = false
      invariantChecked = verity
    } else {
      invariantChecked = invariantChecked && verity
    }
  }

  def isChecked() = {
    invariantChecked
  }
}
