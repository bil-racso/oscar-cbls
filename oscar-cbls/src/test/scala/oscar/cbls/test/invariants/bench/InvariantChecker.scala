package oscar.cbls.test.invariants.bench

import oscar.cbls.invariants.core.propagation.Checker

/**
 *
 * @param verbose
 * @author yoann.guyot@cetic.be
 */
class InvariantChecker(verbose: Int = 0) extends Checker {
  var firstCheck = true
  var invariantChecked = false

  def check(verity: Boolean, traceOption: Option[String]) = {
    if (traceOption.isDefined) {
      val trace = traceOption.get
      if (!verity) println("Counter-example found: " + trace + " is false.")
      else if (verbose > 1) println(trace)
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

