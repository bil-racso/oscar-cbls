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

  val nValues = lower.length
  val rValues = minVal until (minVal + nValues)
  val valuesIdx = 0 until nValues

  // Memorization structure
  val nBound = Array.tabulate(nValues)(i => new ReversibleInt(s, 0))
  val nUnbound = Array.tabulate(nValues)(i => new ReversibleInt(s, 0))
  val unboundForValue = Array.tabulate(nValues)(i => new ReversibleSparseSet(s, 0, X.length - 1))
  val currentlyOk = Array.tabulate(nValues)(i => new ReversibleBoolean(s, false))
  val nCurrentlyOk = new ReversibleInt(s, 0)

  var changeBuffer: Array[Int] = null

  // Things to check when the number of variables with value vi decreases
  private def onTotalDecrease(vi: Int): CPOutcome = {

    if (nBound(vi).value + nUnbound(vi).value == lower(vi)) {
      val it = unboundForValue(vi).iterator
      while (it.hasNext) {
        if (X(it.next()).assign(vi + minVal) == Failure) {
          return Failure
        }
      }
    }
    Suspend
  }

  // Things to check when the number of variables bound to vi increases
  private def onBoundIncrease(vi: Int): CPOutcome = {

    if (nBound(vi).value == upper(vi)) {
      val it = unboundForValue(vi).iterator
      while (it.hasNext) {
        if (X(it.next()).removeValue(vi + minVal) == Failure) {
          return Failure
        }
      }
    }
    Suspend
  }

  // Update the success status for a value in the range
  private def updateOk(vi: Int) = {
    if (!currentlyOk(vi).value &&
      nBound(vi).value >= lower(vi) &&
      nBound(vi).value + nUnbound(vi).value <= upper(vi)) {
      currentlyOk(vi).setValue(true)
      nCurrentlyOk.incr()
    }
  }

  // Compute current outcome
  private def status() =
    if (nCurrentlyOk.value == nValues)
      Success
    else
      Suspend

  override def setup(l: CPPropagStrength): CPOutcome = {

    // Initial counting (the rest is done in the update functions)
    for ((x, i) <- X.zipWithIndex) {
      if (x.isBound && rValues.contains(x.min)) {
        nBound(x.min - minVal).incr()
      }
      for (vi <- valuesIdx) {
        if (x.hasValue(vi + minVal) && !x.isBound) {
          nUnbound(vi).incr()
        } else {
          unboundForValue(vi).removeValue(i)
        }
      }
    }

    // Initializing change buffer
    var bufferSize = 0
    for (x <- X) {
      bufferSize = bufferSize max x.size
    }
    changeBuffer = new Array(bufferSize)

    // We want to get notifications for the crap we do during the first check loop
    for ((x, i) <- X.zipWithIndex) {
      x.callOnChanges(i, whenDomainChanges(_, x, i))
    }

    // First check loop (according to the counts in the initial counting)
    for (vi <- valuesIdx) {

      if (nBound(vi).value + nUnbound(vi).value < lower(vi)) {
        return Failure
      }
      if (nBound(vi).value > upper(vi)) {
        return Failure
      }

      if (onTotalDecrease(vi) == Failure) {
        return Failure
      }
      if (onBoundIncrease(vi) == Failure) {
        return Failure
      }

      updateOk(vi)
    }

    //debug()
    status()
  }

  def whenDomainChanges(delta: DeltaIntVar, x: CPIntVar, i: Int): CPOutcome = {
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      if (rValues.contains(changeBuffer(c))) {
        val vi = changeBuffer(c) - minVal
        nUnbound(vi).decr()
        unboundForValue(vi).removeValue(i)

        if (onTotalDecrease(vi) == Failure) {
          return Failure
        }
        updateOk(vi)
      }
    }
    if (x.isBound) {
      whenBind(x, i)
    } else {
      status()
    }
  }

  def whenBind(x: CPIntVar, i: Int): CPOutcome = {
    if (rValues.contains(x.min)) {
      val vi = x.min - minVal
      nUnbound(vi).decr()
      nBound(vi).incr()
      unboundForValue(vi).removeValue(i)

      if (onBoundIncrease(vi) == Failure) {
        return Failure
      }
      updateOk(vi)
    }
    status()
  }

  private def debug() {
    println("General state:")
    println("Variables:")
    for (x <- X) println(x.toArray.mkString(" "))
    println("nBound:")
    println(nBound.map(_.value).mkString(" "))
    println("nUnbound:")
    println(nUnbound.map(_.value).mkString(" "))
    println("unboundForValue:")
    for (u <- unboundForValue) {
      val it = u.iterator
      while (it.hasNext) print(it.next() + " ")
      println()
    }
    println("currentlyOk:")
    println(currentlyOk.map(_.value).mkString(" "))
    println("nCurrentlyOk: " + nCurrentlyOk.value)
    println()
  }
}