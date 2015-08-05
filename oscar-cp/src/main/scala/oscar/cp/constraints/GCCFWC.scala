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
import oscar.cp.core._
import oscar.cp.core.delta._
import oscar.cp.core.variables._
import oscar.cp.core.CPOutcome._

/**
 * Global Cardinality Constraint
 *
 * Constraint the values minVal+i to appear between low[i] and up[i] times in X
 * @param X The variables to constraint
 * @param minVal The smallest value in the interval; its size is determined by the size of lower and upper
 * @param lower The lower bounds for the occurrences of the values of the interval, in order
 * @param upper The upper bounds for the occurrences of the values of the interval, in order
 * @see SoftGCC
 * @see GCCVar
 *
 * @author Victor Lecomte
 */
class GCCFWC(val X: Array[CPIntVar], val minVal: Int, val lower: Array[Int], val upper: Array[Int])
  extends Constraint(X(0).store, "GCCFWC") {

  idempotent = false

  private[this] val nValues = lower.length
  private[this] val rValues = minVal until (minVal + nValues)
  private[this] val nBounds = 2 * nValues

  // MEMORIZATION STRUCTURE:
  // The number of variables that are bound to the value
  private[this] var nMandatoryRev: Array[ReversibleInt] = null
  // The number of variables that have the value
  private[this] var nPossibleRev: Array[ReversibleInt] = null
  // The number of bounds that are respected
  private[this] var nBoundsOkRev: ReversibleInt = null

  // Change buffer to load the deltas
  private[this] var changeBuffer: Array[Int] = null

  override def setup(l: CPPropagStrength): CPOutcome = {

    // Temporary variables to avoid using reversible variables too much
    val nMandatory = new Array[Int](nValues)
    val nPossible = new Array[Int](nValues)
    var nBoundsOk = 0

    // Initial counting (the rest is done in the update functions)
    var bufferSize = 0
    var i = X.length
    while (i > 0) {
      i -= 1
      val x = X(i)

      // Count the number of variables that are bound to the value
      if (x.isBound) {
        val v = x.min
        if (rValues.contains(v)) {
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
      x.callOnChanges(i, whenDomainChanges(_, x))
    }

    // First check loop (according to the counts in the initial counting)
    var vi = nValues
    while (vi > 0) {
      vi -= 1

      // Few enough variables have the value
      if (nPossible(vi) <= upper(vi)) {
        nBoundsOk += 1
      }
      // Too few variables have the value
      if (nPossible(vi) < lower(vi) ||
        (nPossible(vi) == lower(vi) && whenMinPossible(vi) == Failure)) {
        return Failure
      }

      // Enough variables are bound to the value
      if (nMandatory(vi) >= lower(vi)) {
        nBoundsOk += 1
      }
      // Too many variables are bound to the value
      if (nMandatory(vi) > upper(vi) ||
        (nMandatory(vi) == upper(vi) && whenMaxMandatory(vi) == Failure)) {
        return Failure
      }
    }
    
    // If the all the bounds are ok, the constraint is ok
    if (nBoundsOk == nBounds) {
      return Success
    }

    // Initialize the memorization structure
    nMandatoryRev = Array.tabulate(nValues)(_ => new ReversibleInt(s, 0))
    nPossibleRev = Array.tabulate(nValues)(_ => new ReversibleInt(s, 0))
    nBoundsOkRev = new ReversibleInt(s, 0)

    // Create the buffer
    changeBuffer = new Array(bufferSize)

    // Update the structure
    vi = nValues
    while (vi > 0) {
      vi -= 1
      nMandatoryRev(vi).setValue(nMandatory(vi))
      nPossibleRev(vi).setValue(nPossible(vi))
    }
    nBoundsOkRev.setValue(nBoundsOk)

    Suspend
  }

  /**
   * Update the structure when values are removed from a variable.
   */
  @inline private def whenDomainChanges(delta: DeltaIntVar, x: CPIntVar): CPOutcome = {

    // Treat the value removals
    val i = delta.id
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      val v = changeBuffer(c)
      // If the value removed is one we track
      if (rValues.contains(v)) {
        val vi = v - minVal
        val nPossible = nPossibleRev(vi).decr()

        // If the number of variables that have the value decreases to the upper bound, all good!
        if (nPossible == upper(vi) && nBoundsOkRev.incr() == nBounds) {
          return Success
        }
        // If the number of variables that have the value decreases to the lower bound, that's worrying!
        if (nPossible == lower(vi) && whenMinPossible(vi) == Failure) {
          return Failure
        }
      }
    }

    // Treat the value assignments
    if (x.isBound) {
      val v = x.min
      // If the value assigned is one we track
      if (rValues.contains(v)) {
        val vi = v - minVal
        val nMandatory = nMandatoryRev(vi).incr()

        // If the number of variables that are bound to the value increases to the lower bound, all good!
        if (nMandatory == lower(vi) && nBoundsOkRev.incr() == nBounds) {
          return Success
        }
        // If the number of variables that are bound to the value increases to the upper bound, that's worrying!
        if (nMandatory == upper(vi) && whenMaxMandatory(vi) == Failure) {
          return Failure
        }
      }
    }

    Suspend
  }

  /**
   * When the number of possible variables drops to the lower bound, bind the unbound.
   */
  @inline private def whenMinPossible(vi: Int): CPOutcome = {
    val v = vi + minVal
    var nPossible = 0

    // Bind all the unbound variables that have this value
    var i = X.length
    while (i > 0) {
      i -= 1
      val x = X(i)
      if (x.hasValue(v)) {
        if (!x.isBound) {
          x.assign(v)
        }
        nPossible += 1
      }
    }

    // If there are still too few possible variables, the constraint fails!
    if (nPossible == lower(vi)) Suspend
    else Failure
  }

  /**
   * When the number of mandatory variables reaches the upper bound, drop the unbound.
   */
  @inline private def whenMaxMandatory(vi: Int): CPOutcome = {
    val v = vi + minVal
    var nMandatory = 0

    // Remove the value from the unbound variables that have this value
    var i = X.length
    while (i > 0) {
      i -= 1
      val x = X(i)
      if (x.isBoundTo(v)) {
        nMandatory += 1
      } else if (x.hasValue(v)) {
        x.removeValue(v)
      }
    }

    // If there are still too many mandatory variables, the constraint fails!
    if (nMandatory == upper(vi)) Suspend
    else Failure
  }
}