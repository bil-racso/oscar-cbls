/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/
package oscar.cp.constraints

import oscar.algo.reversible._
import oscar.cp.core.CPOutcome._
import oscar.cp.core._
import oscar.cp.core.delta._
import oscar.cp.core.variables._

/**
 * Global Cardinality Constraint with violation
 *
 * Constraint the values minVal+i to appear between low[i] and up[i] times in X
 * @param X The variables to constraint (at least one)
 * @param minVal The smallest value in the interval; its size is determined by the size of lower and upper
 * @param lower The lower bounds for the occurrences of the values of the interval, in order
 * @param upper The upper bounds for the occurrences of the values of the interval, in order
 * @param viol The violation of the constraints, i.e. the sum of the distances under lower bounds and over upper bounds
 * @see SoftGCC
 *
 * @author Victor Lecomte
 */
class SoftGCCFWC(X: Array[CPIntVar], minVal: Int, lower: Array[Int], upper: Array[Int], viol: CPIntVar)
  extends Constraint(X(0).store, "SoftGCCFWC") {

  idempotent = false

  private[this] val nValues = lower.length
  private[this] val maxVal = minVal + nValues - 1
  private[this] val nVariables = X.length

  // MEMORIZATION STRUCTURE:
  // The number of variables that are bound to the value
  private[this] var nMandatoryRev: Array[ReversibleInt] = null
  // The number of variables that have the value
  private[this] var nPossibleRev: Array[ReversibleInt] = null
  // The best-known minimal/effective violation right now
  private[this] var minViolRev: ReversibleInt = null
  // The best-known maximal/potential violation right now
  private[this] var maxViolRev: ReversibleInt = null

  // Change buffer to load the deltas
  private[this] var changeBuffer: Array[Int] = null

  override def setup(l: CPPropagStrength): CPOutcome = {

    // Temporary variables to avoid using reversible variables too much
    val nMandatory = new Array[Int](nValues)
    val nPossible = new Array[Int](nValues)
    var minViol = 0
    var maxViol = 0

    // Initial counting (the rest is done in the update functions)
    var bufferSize = 0
    var i = nVariables
    while (i > 0) {
      i -= 1
      val x = X(i)

      // Count the number of variables that are bound to the value
      if (x.isBound) {
        val v = x.min
        if (minVal <= v && v <= maxVal) {
          val vi = v - minVal
          nMandatory(vi) += 1
        }
      }
      // Count the number of variables that have the value
      var vi = nValues
      while (vi > 0) {
        vi -= 1
        if (x.hasValue(vi + minVal)) {
          nPossible(vi) += 1
        }
      }

      // Adapt the change buffer size
      if (bufferSize < x.size) {
        bufferSize = x.size
      }

      // Register before the first check loop so that we receive information on what we changed there
      x.callOnChanges(i, delta => whenDomainChanges(delta, x))
    }

    // First violation counting
    var vi = nValues
    while (vi > 0) {
      vi -= 1

      // Effective violations
      if (nPossible(vi) < lower(vi)) {
        minViol += lower(vi) - nPossible(vi)
      }
      if (nMandatory(vi) > upper(vi)) {
        minViol += nMandatory(vi) - upper(vi)
      }

      // Potential violations
      if (nMandatory(vi) < lower(vi)) {
        maxViol += lower(vi) - nMandatory(vi)
      }
      if (nPossible(vi) > upper(vi)) {
        maxViol += nPossible(vi) - upper(vi)
      }
    }

    // First check
    if (viol.updateMin(minViol) == Failure || viol.updateMax(maxViol) == Failure)
      return Failure

    // Initialize the memorization structure
    implicit val context = s
    nMandatoryRev = Array.tabulate(nValues)(vi => ReversibleInt(nMandatory(vi)))
    nPossibleRev = Array.tabulate(nValues)(vi => ReversibleInt(nPossible(vi)))
    minViolRev = ReversibleInt(minViol)
    maxViolRev = ReversibleInt(maxViol)

    // Create the buffer
    changeBuffer = new Array(bufferSize)

    // Everything is up to the closures now
    Success
  }

  /**
   * Update the structure when values are removed from a variable.
   */
  @inline private def whenDomainChanges(delta: DeltaIntVar, x: CPIntVar): CPOutcome = {

    // Treat the value removals
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      val v = changeBuffer(c)
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        val nPossible = nPossibleRev(vi).decr()

        // Worsen the effective violation
        if (nPossible < lower(vi)) {
          if (viol.updateMin(minViolRev.incr()) == Failure)
            return Failure
        }

        // Lessen the potential violation
        if (nPossible >= upper(vi)) {
          if (viol.updateMax(maxViolRev.decr()) == Failure)
            return Failure
        }
      }
    }

    // Treat the value assignments
    if (x.isBound) {
      val v = x.min
      // If the value assigned is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal
        val nMandatory = nMandatoryRev(vi).incr()

        // Worsen the effective violation
        if (nMandatory > upper(vi)) {
          if (viol.updateMin(minViolRev.incr()) == Failure)
            return Failure
        }

        // Lessen the potential violation
        if (nMandatory <= lower(vi)) {
          if (viol.updateMax(maxViolRev.decr()) == Failure)
            return Failure
        }
      }
    }

    Suspend
  }
}