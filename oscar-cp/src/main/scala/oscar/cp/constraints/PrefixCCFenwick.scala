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

import oscar.algo.reversible.{ReversibleBoolean, ReversibleInt}
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.{CPPropagStrength, CPOutcome, Constraint}
import oscar.cp.core.variables.CPIntVar
import CPOutcome._

/**
 * Cardinality constraint on prefixes of a variable array.
 * Strong version of PrefixCCSegments using Fenwick trees, where the pruning is optimal (AC) for each value on the
 * subset of the bounds that concern that value. For the parameters of the constructor, see PrefixCCSegments.
 * @author Victor Lecomte
 */
class PrefixCCFenwick(X: Array[CPIntVar], minVal: Int, lowerLists: Array[Array[(Int, Int)]],
                      upperLists: Array[Array[(Int, Int)]])
  extends Constraint(X(0).store, "PrefixCCFenwick") {
  /*
   * Idea: This constraint keeps in a reversible structure the list of all the best-known lower bounds and upper bounds
   * according to the bounds given in the input and the state of the variables. Those bounds are the best that can be
   * deduced from that information, and the pruning that results from it is, as mentioned above, optimal according to
   * those bounds.
   *
   * It prunes when: the lower bound and the upper bound become equal at both bounds around an unbound variable that
   * still has the value ("undecided" variable). It removes the value if the difference is zero (a "flat") and assigns
   * the variable to the value if the difference is one (a "slope"). There are no other possible differences, because,
   * again, the bounds are the best-known ones.
   *
   * The structures: Some reversible variables for storing the differences between the consecutive lower bounds and the
   * consecutive upper bounds. A reversible Fenwick tree to keep the gap between the lower bounds and upper bounds in
   * logarithmic time. The "number of hazards", that is the number of "slopes" in lower bounds and "flats" in upper
   * bounds around "undecided" variables, is a magical number that determines when the constraint is definitely
   * successful when it falls to zero.
   *
   * The tricky part: How to update the lower bounds and upper bounds as much as we can when an update is received. That
   * is the reason why the whenDomainChanges method is so long (but probably not a very good excuse).
   */

  /** Number of variables */
  private[this] val nVariables = X.length
  /** Number of values for which we are given bounds */
  private[this] val nValues = lowerLists.length
  /** Largest value */
  private[this] val maxVal = minVal + nValues - 1

  /** Buffer to read changes from deltas */
  private[this] var changeBuffer: Array[Int] = null

  /** List of the best known lower bounds for every value and prefix, used in initialization */
  private[this] val lower = Array.tabulate(nValues, nVariables + 1)((vi, i) => 0)
  /** List of the best known upper bounds for every value and prefix, used in initialization */
  private[this] val upper = Array.tabulate(nValues, nVariables + 1)((vi, i) => i)

  /** Difference between the best-known lower bounds before and after that variable */
  private[this] val lowerDiffRev = Array.ofDim[ReversibleInt](nValues, nVariables)
  /** Difference between the best-known upper bounds before and after that variable */
  private[this] val upperDiffRev = Array.ofDim[ReversibleInt](nValues, nVariables)

  /**
   * Fenwick tree memorizing the difference between the lower bound and the upper bound for every value and prefix.
   * Allows fast updates of the difference in bounds between two consecutive prefixes.
   */
  private[this] val gapFenwick = Array.ofDim[ReversibleInt](nValues, nVariables + 1)

  // Linked lists of unbound variables that have the value
  /** Previous unbound variable that has the value */
  private[this] val prevRev = Array.ofDim[ReversibleInt](nValues, nVariables)
  /** Next unbound variable that has the value */
  private[this] val nextRev = Array.ofDim[ReversibleInt](nValues, nVariables)

  /**
   * The number of "hazards" is a concept that describes the capacity of future variable assignments to cause failure,
   * based on the differences between successive best-known bounds where an unbound variable that has the value is
   * located. Let us call a "flat" a difference of 0 and a "slope" a difference of 1 (there are no other possible
   * differences). Then every flat in the upper bounds where an unbound variable has the value is a hazard, and
   * every slope in the lower bounds where an unbound variable has the value is a hazard too. If there are no hazards,
   * the constraint is successful.
   */
  private[this] var nHazardsRev: ReversibleInt = null
  /** Temporary variable to store the number of hazards and avoid updating reversible variables. */
  private[this] var nHazardsTmp = 0

  // ============
  // INIT METHODS
  // ============

  override def setup(l: CPPropagStrength): CPOutcome = {

    // Whether bounds given in the input are feasible
    val feasibleLower = (index: Int, value: Int) => value <= index
    val feasibleUpper = (index: Int, value: Int) => value >= 0

    val keepMin = (i: Int, j: Int) => i min j
    val keepMax = (i: Int, j: Int) => i max j
    val flat = (value: Int) => value
    val slopeForwards = (value: Int) => value + 1
    val slopeBackwards = (value: Int) => value - 1

    if (readArguments(lower, lowerLists, feasibleLower) == Failure)
      return Failure
    if (readArguments(upper, upperLists, feasibleUpper) == Failure)
      return Failure

    cleverFill(lower, flat, slopeBackwards, keepMax)
    cleverFill(upper, slopeForwards, flat, keepMin)

    if (testAndDeduceBetweenValues() == Failure)
      return Failure

    registerForChanges()

    if (initAndPrune() == Failure)
      return Failure

    // Everything is up to the registered closures now
    Success
  }

  /**
   * Copies the bounds given into arrays or fails if the bounds are unfeasible
   * @param bounds The arrays in which to copy the bounds
   * @param boundLists The given lists of bounds
   * @param feasible The feasibility criterion in terms of the index and the value of the bound
   * @return [[Failure]] if one of the bounds in unfeasible, [[Suspend]] otherwise
   */
  private def readArguments(bounds: Array[Array[Int]], boundLists: Array[Array[(Int, Int)]],
                            feasible: (Int, Int) => Boolean): CPOutcome = {
    var vi = nValues
    while (vi > 0) {
      vi -= 1

      var bound = boundLists(vi).length
      while (bound > 0) {
        bound -= 1
        val (index, value) = boundLists(vi)(bound)

        if (index < 0 || index > nVariables)
          throw new IllegalArgumentException("Bound cutoff out of range: " + index)
        else if (!feasible(index, value))
          return Failure

        bounds(vi)(index) = value
      }
    }

    Suspend
  }

  /**
   * Fills the arrays of bounds according to the bounds already present and the states of the variables
   * @param bounds The arrays of bounds to work on
   * @param rightThroughUnbound What we know on the value of a bound when we know the value of the bound for the
   *                            previous prefix when between the two is an unbound variable that has the value
   * @param leftThroughUnbound What we know on the value of a bound when we know the value of the bound for the next
   *                           prefix when between the two is an unbound variable that has the value
   * @param keep Tells which value to keep when there are two possible values for a bound
   */
  private def cleverFill(bounds: Array[Array[Int]], rightThroughUnbound: Int => Int, leftThroughUnbound: Int => Int,
                         keep: (Int, Int) => Int) {
    var vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = minVal + vi

      var i = 0
      // Fill from left to right
      while (i < nVariables) {
        val cur = bounds(vi)(i)
        // When the variable is either bound to v or does not have v,
        // it is clear what to deduce of the next bound.
        val next = {
          if (X(i).isBoundTo(v))
            cur + 1
          else if (!X(i).hasValue(v))
            cur
          else
            rightThroughUnbound(cur)
        }
        i += 1
        bounds(vi)(i) = keep(bounds(vi)(i), next)
      }
      // Fill from right to left
      while (i > 0) {
        val cur = bounds(vi)(i)
        i -= 1
        // When the variable is either bound to v or does not have v,
        // it is clear what to deduce of the previous bound.
        val prev = {
          if (X(i).isBoundTo(v))
            cur - 1
          else if (!X(i).hasValue(v))
            cur
          else
            leftThroughUnbound(cur)
        }
        bounds(vi)(i) = keep(bounds(vi)(i), prev)
      }
    }
  }

  /**
   * Does some basic tests on the bounds and deduces bounds based on the bounds for other values
   * @return [[Failure]] if the bounds are found to be unfeasible, [[Suspend]] otherwise
   */
  private def testAndDeduceBetweenValues(): CPOutcome = {
    var i = nVariables
    while (i > 0) {

      // Compute the sums
      var lowerSum = 0
      var upperSum = 0
      var vi = nValues
      while (vi > 0) {
        vi -= 1
        if (lower(vi)(i) > upper(vi)(i))
          return Failure
        lowerSum += lower(vi)(i)
        upperSum += upper(vi)(i)
      }

      // Test the sums
      if (lowerSum > i || upperSum < i)
        return Failure

      // Deduce some bounds
      vi = nValues
      while (vi > 0) {
        vi -= 1
        // The lower bound will be at least the number of variables minus the sum of all other upper bounds
        lower(vi)(i) = lower(vi)(i) max (i - upperSum + upper(vi)(i))
        // The upper bound will be at most the number of variables minus the sum of all other lower bounds
        upper(vi)(i) = upper(vi)(i) min (i - lowerSum + lower(vi)(i))
      }
      i -= 1
    }
    Suspend
  }

  /**
   * Registers for changes in the variables with a closure.
   * Note that the closure tests whether the number of hazards has fallen to zero before calling the update method.
   */
  private def registerForChanges() {
    var i = nVariables
    var bufferSize = 0
    while (i > 0) {
      i -= 1
      val x = X(i)
      bufferSize = bufferSize max x.size
      x.callOnChanges(i, delta => {
        val nHazardsOld = nHazardsRev.value
        if (nHazardsOld == 0) Success
        else whenDomainChanges(x, delta, nHazardsOld)
      })
    }
    changeBuffer = Array.ofDim[Int](bufferSize)
  }

  /**
   * Initializes the reversible variables for the consecutive differences, the fenwick tree and the number of hazards.
   * Performs a first round of pruning.
   * @return [[Failure]] if a failure is detected, [[Suspend]] otherwise
   */
  private def initAndPrune(): CPOutcome = {
    implicit val store = s

    // Whether a variable is unbound and has the value
    val unboundTmp = Array.ofDim[Boolean](nValues, nVariables)
    var vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = vi + minVal
      var i = nVariables
      while (i > 0) {
        i -= 1
        unboundTmp(vi)(i) = X(i).hasValue(v) && !X(i).isBound
      }
    }

    vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = vi + minVal

      var lastUnbound = -1
      // Remember the current gap in order to build the Fenwick tree
      val gap = Array.ofDim[Int](nVariables + 1)

      var i = 0
      while (i < nVariables) {

        val lowerDiff = lower(vi)(i + 1) - lower(vi)(i)
        val upperDiff = upper(vi)(i + 1) - upper(vi)(i)
        lowerDiffRev(vi)(i) = ReversibleInt(lowerDiff)
        upperDiffRev(vi)(i) = ReversibleInt(upperDiff)
        gap(i + 1) = gap(i) + upperDiff - lowerDiff

        if (unboundTmp(vi)(i)) {
          // Update the number of hazards
          if (lowerDiff == 1)
            nHazardsTmp += 1
          if (upperDiff == 0)
            nHazardsTmp += 1

          // Update the list
          prevRev(vi)(i) = ReversibleInt(lastUnbound)
          if (lastUnbound != -1)
            nextRev(vi)(lastUnbound) = ReversibleInt(i)
          lastUnbound = i

          // If there are flats or slopes with no gap, we can prune directly
          if (gap(i) == 0 && gap(i + 1) == 0) {
            if (lowerDiff == 0 && X(i).removeValue(v) == Failure)
              return Failure
            if (lowerDiff == 1 && X(i).assign(v) == Failure)
              return Failure
          }
        }
        i += 1
        gapFenwick(vi)(i) = ReversibleInt(gap(i) - gap(i - (i & (-i))))
      }
      if (lastUnbound != -1)
        nextRev(vi)(lastUnbound) = ReversibleInt(nVariables)
    }

    nHazardsRev = ReversibleInt(nHazardsTmp)

    Suspend
  }

  // ==============
  // UPDATE METHODS
  // ==============

  /**
   * Updates the structures and prunes according to the changes made in the variable
   * @param x The variable that was changed
   * @param delta The values that were removed from it
   * @param nHazardsOld The value of the number of hazards before the update
   * @return [[Failure]] if the pruning caused a failure, [[Suspend]] otherwise
   */
  @inline private def whenDomainChanges(x: CPIntVar, delta: DeltaIntVar, nHazardsOld: Int): CPOutcome = {
    val i = delta.id
    nHazardsTmp = nHazardsOld

    // Treat the value removals
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      val v = changeBuffer(c)
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal

        val lowerDiff = lowerDiffRev(vi)(i).value
        val upperDiff = upperDiffRev(vi)(i).value
        val fenwick = gapFenwick(vi)

        var gapLeft = 0
        var gapRight = 0

        // We only need to know the gap if some bounds are going to change
        if (lowerDiff == 1 || upperDiff == 1) {
          gapLeft = getGap(fenwick, i)
          gapRight = gapLeft + upperDiff - lowerDiff
        }

        // If the value is removed but there was a slope in the lower bounds,
        // the lower bound on the left and others that were linked to it have to be raised.
        if (lowerDiff == 1) {
          lowerDiffRev(vi)(i).setValue(0)

          // Eliminate while the gap is zero
          var eliminate = gapLeft == 1
          var cur = i
          nHazardsTmp += 1
          do {
            cur = prevRev(vi)(cur).value
            if (eliminate) {
              if (upperDiffRev(vi)(cur).value == 1) {
                if (X(cur).assign(v) == Failure)
                  return Failure
                nHazardsTmp -= 1
              } else {
                eliminate = false
              }
            }
          } while (lowerDiffRev(vi)(cur).value != 0)
          lowerDiffRev(vi)(cur).setValue(1)
          decrGap(fenwick, cur + 1)

          if (nHazardsTmp == 0) {
            nHazardsRev.setValue(0)
            return Success
          }
        }

        // If the value is removed but there was a slope in the upper bounds,
        // the upper bound on the right and others that were linked to it have to be lowered.
        if (upperDiff == 1) {
          upperDiffRev(vi)(i).setValue(0)

          // Eliminate while the gap is zero
          var eliminate = gapRight == 1
          var cur = i
          do {
            cur = nextRev(vi)(cur).value
            if (eliminate && cur != nVariables) {
              if (lowerDiffRev(vi)(cur).value == 1) {
                if (X(cur).assign(v) == Failure)
                  return Failure
                nHazardsTmp -= 1
              } else {
                eliminate = false
              }
            }
          } while (cur != nVariables && upperDiffRev(vi)(cur).value != 0)
          if (cur != nVariables) {
            nHazardsTmp -= 1
            upperDiffRev(vi)(cur).setValue(1)
            incrGap(fenwick, cur + 1)
          }

          if (nHazardsTmp == 0) {
            nHazardsRev.setValue(0)
            return Success
          }
        }

        if (lowerDiff == 1 && upperDiff == 0)
          incrGap(fenwick, i + 1)
        else if (lowerDiff == 0 && upperDiff == 1)
          decrGap(fenwick, i + 1)

        // Remove the variable from the list of unbound variables that have the value
        removeUnbound(vi, i)
      }
    }

    // Treat the assignments
    if (x.isBound) {
      val v = x.min
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal

        val lowerDiff = lowerDiffRev(vi)(i).value
        val upperDiff = upperDiffRev(vi)(i).value
        val fenwick = gapFenwick(vi)

        var gapLeft = 0
        var gapRight = 0

        // We only need to know the gap if some bounds are going to change
        if (lowerDiff == 0 || upperDiff == 0) {
          gapLeft = getGap(fenwick, i)
          gapRight = gapLeft + upperDiff - lowerDiff
        }

        // If the value is assigned but there was a flat in the lower bounds,
        // the lower bound on the right and others that were linked to it have to be raised.
        if (lowerDiff == 0) {
          lowerDiffRev(vi)(i).setValue(1)

          // Eliminate while the gap is zero
          var eliminate = gapRight == 1
          var cur = i
          do {
            cur = nextRev(vi)(cur).value
            if (eliminate && cur != nVariables) {
              if (upperDiffRev(vi)(cur).value == 0) {
                if (X(cur).removeValue(vi) == Failure)
                  return Failure
                nHazardsTmp -= 1
              } else {
                eliminate = false
              }
            }
          } while (cur != nVariables && lowerDiffRev(vi)(cur).value != 1)
          if (cur != nVariables) {
            nHazardsTmp -= 1
            lowerDiffRev(vi)(cur).setValue(0)
            incrGap(fenwick, cur + 1)
          }

          if (nHazardsTmp == 0) {
            nHazardsRev.setValue(0)
            return Success
          }
        }

        // If the value is assigned but there was a flat in the upper bounds,
        // the upper bound on the left and others that were linked to it have to be lowered.
        if (upperDiff == 0) {
          upperDiffRev(vi)(i).setValue(1)

          // Eliminate while the gap is zero
          var eliminate = gapLeft == 1
          var cur = i
          nHazardsTmp += 1
          do {
            cur = prevRev(vi)(cur).value
            if (eliminate) {
              if (lowerDiffRev(vi)(cur).value == 0) {
                if (X(cur).removeValue(vi) == Failure)
                  return Failure
                nHazardsTmp -= 1
              } else {
                eliminate = false
              }
            }
          } while (upperDiffRev(vi)(cur).value != 1)
          upperDiffRev(vi)(cur).setValue(0)
          decrGap(fenwick, cur + 1)

          if (nHazardsTmp == 0) {
            nHazardsRev.setValue(0)
            return Success
          }
        }

        if (lowerDiff == 1 && upperDiff == 0)
          incrGap(fenwick, i + 1)
        else if (lowerDiff == 0 && upperDiff == 1)
          decrGap(fenwick, i + 1)

        // Remove the variable from the list of unbound variables that have the value
        removeUnbound(vi, i)
      }
    }

    if (nHazardsTmp != nHazardsOld)
      nHazardsRev.setValue(nHazardsTmp)

    Suspend
  }

  /**
   * Returns the cumulative sum of the Fenwick tree before index i.
   * So in this case, the gap between the lower and upper bounds.
   * @param fenwick The Fenwick tree
   * @param i The end index
   * @return The gap between the lower and upper bounds for the prefix [0,i[
   */
  @inline private def getGap(fenwick: Array[ReversibleInt], i: Int): Int = {
    var sum = 0
    var cur = i
    while (cur > 0) {
      sum += fenwick(cur).value
      cur -= (cur & -cur)
    }
    sum
  }

  /**
   * Increases the gap by one at an index
   * @param fenwick The Fenwick tree
   * @param i The index where the increase takes place
   */
  @inline private def incrGap(fenwick: Array[ReversibleInt], i: Int) {
    var cur = i
    while (cur <= nVariables) {
      fenwick(cur).incr()
      cur += (cur & -cur)
    }
  }

  /**
   * Decreases the gap by one at an index
   * @param fenwick The Fenwick tree
   * @param i The index where the decrease takes place
   */
  @inline private def decrGap(fenwick: Array[ReversibleInt], i: Int) {
    var cur = i
    while (cur <= nVariables) {
      fenwick(cur).decr()
      cur += (cur & -cur)
    }
  }

  /**
   * Remove the variable from the list of unbound variables that have the value
   * @param vi The index of the value
   * @param i The index of the variable
   */
  @inline private def removeUnbound(vi: Int, i: Int): Unit = {
    val prev = prevRev(vi)(i).value
    val next = nextRev(vi)(i).value
    if (prev != -1)
      nextRev(vi)(prev).setValue(next)
    if (next != nVariables)
      prevRev(vi)(next).setValue(prev)
  }
}