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

import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.delta.DeltaIntVar
import oscar.cp.core.{CPPropagStrength, CPOutcome, Constraint}
import oscar.cp.core.variables.CPIntVar
import CPOutcome._

/**
 * @author Victor Lecomte
 */
class PrefixCCFWC(X: Array[CPIntVar], minVal: Int, lowerLists: Array[Array[(Int, Int)]], upperLists: Array[Array[(Int, Int)]])
  extends Constraint(X(0).store, "PrefixCCFWC") {

  // Handy structures for memorization

  private class SegmentStructure() {
    var full: Array[Int] = null

    val boundIdx = Array.ofDim[Int](nVariables + 1)
    val boundVal = Array.ofDim[Int](nVariables + 1)
    var nBounds = 0
    var nIntervals = 0
    var lastIdx = 0

    var intervalOf: Array[Int] = null
    var parentRev: Array[ReversibleInt] = null
    var untilCriticalRev: Array[ReversibleInt] = null
    var prevRev: Array[ReversibleInt] = null
    var rightLimitRev: Array[ReversibleInt] = null
  }

  private class TempStructure() {
    var parent: Array[Int] = null
    var untilCritical: Array[Int] = null
    var rightLimit: Array[Int] = null
  }

  private class UnboundList() {
    var firstRev: ReversibleInt = null
    var prevRev: Array[ReversibleInt] = null
    var nextRev: Array[ReversibleInt] = null
  }

  private class TempList(size: Int) {
    var first = size
    var last = -1
    val prev = Array.fill(size)(-1)
    val next = Array.fill(size)(size)
    val contains = Array.fill(size)(false)
  }

  private[this] val nVariables = X.length
  private[this] var nRelevantVariables = 0
  private[this] val nValues = lowerLists.length
  private[this] val maxVal = minVal + nValues - 1

  private[this] val lower = Array.fill(nValues)(new SegmentStructure())
  private[this] val upper = Array.fill(nValues)(new SegmentStructure())
  private[this] val unbound = Array.fill(nValues)(new UnboundList())

  private[this] var changeBuffer: Array[Int] = null


  // ============
  // INIT METHODS
  // ============

  override def setup(l: CPPropagStrength): CPOutcome = {

    val feasibleLower = (index: Int, value: Int) => value <= index
    val feasibleUpper = (index: Int, value: Int) => value >= 0

    var vi = nValues
    while (vi > 0) {
      vi -= 1

      lower(vi).full = Array.tabulate(nVariables + 1)(i => 0)
      upper(vi).full = Array.tabulate(nVariables + 1)(i => i)

      if (readArguments(lower(vi), lowerLists(vi), feasibleLower) == Failure) return Failure
      if (readArguments(upper(vi), upperLists(vi), feasibleUpper) == Failure) return Failure
    }

    filterBounds()
    fillBounds()
    if (testAndDeduceBounds() == Failure) return Failure
    filterBounds()

    initAndCheck()
  }

  private def readArguments(st: SegmentStructure, boundList: Array[(Int, Int)],
                            feasible: (Int, Int) => Boolean): CPOutcome = {
    var bound = boundList.length
    while (bound > 0) {
      bound -= 1
      val (index, value) = boundList(bound)
      if (index < 0 || index > nVariables) {
        throw new IllegalArgumentException("Bound cutoff out of range: " + index)
      } else if (!feasible(index, value)) {
        return Failure
      }
      st.full(index) = value
    }

    Suspend
  }

  // ---------
  // Filtering
  // ---------

  private def filterBounds() {
    nRelevantVariables = 0

    val filterFlat = (prevIdx: Int, prevVal: Int, nextIdx: Int, nextVal: Int) => nextVal > prevVal
    val filterSlope = (prevIdx: Int, prevVal: Int, nextIdx: Int, nextVal: Int) => nextVal - prevVal < nextIdx - prevIdx

    var vi = nValues
    while (vi > 0) {
      vi -= 1

      filterGeneric(lower(vi), filterSlope, filterFlat)
      filterGeneric(upper(vi), filterFlat, filterSlope)
      nRelevantVariables = nRelevantVariables max lower(vi).lastIdx max upper(vi).lastIdx
    }
  }

  private def filterGeneric(st: SegmentStructure,
                            prevFilter: (Int, Int, Int, Int) => Boolean,
                            nextFilter: (Int, Int, Int, Int) => Boolean) {
    import st._

    // Adding lower and upper bound 0 at 0, for convenience.
    boundIdx(0) = 0
    boundVal(0) = 0
    nBounds = 1

    def lastIdx = boundIdx(nBounds - 1)
    def lastVal = boundVal(nBounds - 1)

    var i = 1
    while (i <= nVariables) {
      if (nextFilter(lastIdx, lastVal, i, full(i))) {
        while (nBounds > 1 && !prevFilter(lastIdx, lastVal, i, full(i))) {
          nBounds -= 1
        }
        boundIdx(nBounds) = i
        boundVal(nBounds) = full(i)
        nBounds += 1
      }
      i += 1
    }

    (nBounds, nBounds - 1, lastIdx)
    nIntervals = nBounds - 1
    st.lastIdx = lastIdx
  }

  // -------
  // Filling
  // -------

  private def fillBounds() {
    val flatFill = (baseIdx: Int, baseVal: Int, i: Int) => baseVal
    val slopeFill = (baseIdx: Int, baseVal: Int, i: Int) => baseVal + i - baseIdx
    val lowerSplit = (prevIdx: Int, prevVal: Int, nextIdx: Int, nextVal: Int) => nextIdx - nextVal + prevVal
    val upperSplit = (prevIdx: Int, prevVal: Int, nextIdx: Int, nextVal: Int) => prevIdx + nextVal - prevVal

    var vi = nValues
    while (vi > 0) {
      vi -= 1

      fillGeneric(lower(vi), lowerSplit, flatFill, slopeFill)
      fillGeneric(upper(vi), upperSplit, slopeFill, flatFill)
    }
  }

  private def fillGeneric(st: SegmentStructure,
                          splitAt: (Int, Int, Int, Int) => Int,
                          leftFill: (Int, Int, Int) => Int, rightFill: (Int, Int, Int) => Int) {
    import st._

    var i = nVariables + 1
    var lowerI = nBounds
    while (lowerI > 0) {
      lowerI -= 1
      val split = {
        if (lowerI == nIntervals) nVariables + 1
        else splitAt(boundIdx(lowerI), boundVal(lowerI), boundIdx(lowerI + 1), boundVal(lowerI + 1))
      }
      while (i > split) {
        i -= 1
        full(i) = rightFill(boundIdx(lowerI + 1), boundVal(lowerI + 1), i)
      }
      while (i > boundIdx(lowerI)) {
        i -= 1
        full(i) = leftFill(boundIdx(lowerI), boundVal(lowerI), i)
      }
    }
  }

  // ---------
  // Deduction
  // ---------

  private def testAndDeduceBounds(): CPOutcome = {
    // Perform some deduction based on other values' lower and upper bounds
    var i = nVariables
    while (i > 0) {
      // Compute the sums
      var lowerSum = 0
      var upperSum = 0
      var vi = nValues
      while (vi > 0) {
        vi -= 1
        if (lower(vi).full(i) > upper(vi).full(i)) return Failure
        lowerSum += lower(vi).full(i)
        upperSum += upper(vi).full(i)
      }
      // Test the sums
      if (lowerSum > i || upperSum < i) return Failure
      // Deduce some bounds
      vi = nValues
      while (vi > 0) {
        vi -= 1
        // The lower bound will be at least the number of variables minus the sum of all other upper bounds
        lower(vi).full(i) = lower(vi).full(i) max (i - upperSum + upper(vi).full(i))
        // The upper bound will be at most the number of variables minus the sum of all other lower bounds
        upper(vi).full(i) = upper(vi).full(i) min (i - lowerSum + lower(vi).full(i))
      }
      i -= 1
    }
    Suspend
  }

  // -------------------------
  // Initial counts and checks
  // -------------------------

  private def initAndCheck(): CPOutcome = {

    val lowerTmp = Array.fill(nValues)(new TempStructure())
    val upperTmp = Array.fill(nValues)(new TempStructure())

    val criticalInitLower = (prevVal: Int, nextVal: Int) => prevVal - nextVal
    val criticalInitUpper = (prevVal: Int, nextVal: Int) => nextVal - prevVal

    // Give initial values to the structures
    var vi = nValues
    while (vi > 0) {
      vi -= 1
      initTemp(lower(vi), lowerTmp(vi), criticalInitLower)
      initTemp(upper(vi), upperTmp(vi), criticalInitUpper)
    }

    // Adapt the size of the buffer
    var bufferSize = 0
    var i = nRelevantVariables
    while (i > 0) {
      i -= 1
      bufferSize = bufferSize max X(i).size
    }
    changeBuffer = Array.ofDim[Int](bufferSize)

    // Create the linked list of unbound variables
    val unboundTmp = Array.fill(nValues)(new TempList(nRelevantVariables))
    initialCount(lowerTmp, upperTmp, unboundTmp)

    // Initial checks
    vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = vi + minVal

      if (initialCheck(lower(vi), lowerTmp(vi), unboundTmp(vi), x => x.assign(v)) == Failure) return Failure
      if (initialCheck(upper(vi), upperTmp(vi), unboundTmp(vi), x => x.removeValue(v)) == Failure) return Failure
    }

    // Copy the temporary values into the reversible arrays
    vi = nValues
    while (vi > 0) {
      vi -= 1

      copyToRev(lower(vi), lowerTmp(vi))
      copyToRev(upper(vi), upperTmp(vi))
      copyListToRev(unbound(vi), unboundTmp(vi))
    }

    Success
  }

  private def initialCount(lowerTmp: Array[TempStructure], upperTmp: Array[TempStructure],
                           unboundTmp: Array[TempList]) {
    // Initial count
    var i = 0
    while (i < nRelevantVariables) {
      val x = X(i)

      if (x.isBound) {
        val v = x.min
        if (minVal <= v && v <= maxVal) {
          val vi = v - minVal
          if (i < lower(vi).lastIdx) {
            lowerTmp(vi).untilCritical(lower(vi).intervalOf(i)) += 1
          }
          if (i < upper(vi).lastIdx) {
            upperTmp(vi).untilCritical(upper(vi).intervalOf(i)) -= 1
          }
        }
      } else {
        var c = x.fillArray(changeBuffer)
        while (c > 0) {
          c -= 1
          val v = changeBuffer(c)
          if (minVal <= v && v <= maxVal) {
            val vi = v - minVal
            if (i < lower(vi).lastIdx) {
              lowerTmp(vi).untilCritical(lower(vi).intervalOf(i)) += 1
            }

            // Fill the linked list of unbound variables
            val list = unboundTmp(vi)
            list.contains(i) = true
            if (list.first == nRelevantVariables) {
              list.first = i
            } else {
              list.next(list.last) = i
              list.prev(i) = list.last
            }
            list.last = i
          }
        }
      }

      // Register before the first check loop so that we receive information on what we changed there
      x.callOnChanges(i, delta => whenDomainChanges(delta, x))
      i += 1
    }
  }

  private def initialCheck(st: SegmentStructure, tmp: TempStructure, list: TempList,
                           action: CPIntVar => CPOutcome): CPOutcome = {
    import st._
    // Merge as much as possible, intentionally backwards!
    var inter = nIntervals
    while (inter > 1) {
      inter -= 1

      if (tmp.untilCritical(inter) <= 0) {
        // TODO: decrement nIntervals probably??
        tmp.parent(inter) = inter - 1
        tmp.untilCritical(inter - 1) += tmp.untilCritical(inter)
        tmp.rightLimit(inter - 1) = tmp.rightLimit(inter)
      }
    }

    if (nIntervals > 0) {
      // If some of the leftmost constraints are already decided
      if (tmp.untilCritical(0) < 0) return Failure
      else if (tmp.untilCritical(0) == 0) {
        // Try to assign or remove the unbound
        var i = list.first
        while (i < tmp.rightLimit(0)) {
          if (action(X(i)) == Failure) return Failure
          i = list.next(i)
        }

        // If this is the only interval remaining
        if (tmp.rightLimit(0) == lastIdx) {
          tmp.parent(0) = -1
        }
        // Otherwise, merge it to the right
        else {
          val next = intervalOf(tmp.rightLimit(0))
          tmp.parent(next) = 0
          tmp.untilCritical(0) = tmp.untilCritical(next)
          tmp.rightLimit(0) = tmp.rightLimit(next)
        }
      }
    }

    // Redirect variables
    inter = nIntervals
    var i = lastIdx
    while (inter > 0) {
      inter -= 1
      if (tmp.parent(inter) == inter) {
        while (i > boundIdx(inter)) {
          i -= 1
          intervalOf(i) = inter
        }
      }
    }

    Suspend
  }

  // ------------------------------
  // Temporary structure management
  // ------------------------------

  private def initTemp(st: SegmentStructure, tmp: TempStructure, criticalInit: (Int, Int) => Int) {
    import st._

    intervalOf = Array.ofDim[Int](lastIdx)
    tmp.parent = Array.tabulate(nIntervals)(inter => inter)
    tmp.untilCritical = Array.tabulate(nIntervals)(inter => criticalInit(boundVal(inter), boundVal(inter + 1)))
    tmp.rightLimit = Array.tabulate(nIntervals)(inter => boundIdx(inter + 1))

    var inter = nIntervals
    var i = lastIdx
    while (inter > 0) {
      inter -= 1
      while (i > boundIdx(inter)) {
        i -= 1
        intervalOf(i) = inter
      }
    }
  }

  private def copyToRev(st: SegmentStructure, tmp: TempStructure) {
    import st._
    // Initialize the reversible arrays
    parentRev = Array.ofDim[ReversibleInt](nIntervals)
    untilCriticalRev = Array.ofDim[ReversibleInt](nIntervals)
    prevRev = Array.ofDim[ReversibleInt](nIntervals)
    rightLimitRev = Array.ofDim[ReversibleInt](nIntervals)

    var inter = 0
    var prevInter = -1
    while (inter < nIntervals) {
      if (tmp.parent(inter) == inter) {
        parentRev(inter) = new ReversibleInt(s, tmp.parent(inter))
        untilCriticalRev(inter) = new ReversibleInt(s, tmp.untilCritical(inter))
        prevRev(inter) = new ReversibleInt(s, prevInter)
        rightLimitRev(inter) = new ReversibleInt(s, tmp.rightLimit(inter))
        prevInter = inter
      }
      inter += 1
    }
  }

  private def copyListToRev(list: UnboundList, tmpList: TempList) {
    list.firstRev = new ReversibleInt(s, tmpList.first)
    list.prevRev = Array.tabulate(nRelevantVariables)(i =>
      if (tmpList.contains(i)) new ReversibleInt(s, tmpList.prev(i))
      else null
    )
    list.nextRev = Array.tabulate(nRelevantVariables)(i =>
      if (tmpList.contains(i)) new ReversibleInt(s, tmpList.next(i))
      else null
    )
  }


  // ==============
  // UPDATE METHODS
  // ==============

  @inline private def whenDomainChanges(delta: DeltaIntVar, x: CPIntVar): CPOutcome = {
    val i = delta.id

    // Treat the value removals
    var c = delta.fillArray(changeBuffer)
    while (c > 0) {
      c -= 1
      val v = changeBuffer(c)
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal

        if (onUpdate(i, lower(vi), unbound(vi), otherVar => otherVar.assign(v)) == Failure) return Failure
      }
    }

    if (x.isBound) {
      val v = x.min
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal

        if (onUpdate(i, upper(vi), unbound(vi), otherVar => otherVar.removeValue(v)) == Failure) return Failure
      }
    }

    Suspend
  }

  @inline private def onUpdate(i: Int, st: SegmentStructure, list: UnboundList,
                               action: CPIntVar => CPOutcome): CPOutcome = {
    import st._

    if (i < lastIdx) {
      removeUnbound(list, i)

      val inter = findParent(parentRev, intervalOf(i))
      if (inter != -1) {
        val untilCritical = untilCriticalRev(inter).decr()

        if (untilCritical == 0) {
          val directPrev = prevRev(inter).value

          // If we are at zero, we eliminate unbound and merge with the next interval
          if (directPrev == -1) {

            // Assign or remove every unbound
            val middleLimit = rightLimitRev(inter).value
            var unboundI = list.firstRev.value
            while (unboundI < middleLimit) {
              if (action(X(unboundI)) == Failure) return Failure
              unboundI = list.nextRev(unboundI).value
            }

            // Merge with next if possible
            if (middleLimit != lastIdx) {
              val next = findParent(parentRev, intervalOf(middleLimit))

              // Compute limits to guess the preferable merge side
              val rightLimit = rightLimitRev(next).value

              // Left merge
              if (2 * middleLimit > rightLimit) {
                parentRev(next).setValue(inter)
                untilCriticalRev(inter).setValue(untilCriticalRev(next).value)
                rightLimitRev(inter).setValue(rightLimit)
              }
              // Right merge
              else {
                parentRev(inter).setValue(next)
                prevRev(next).setValue(-1)
              }
            }
          }

          // Otherwise, we merge with the previous interval
          else {
            val prev = findParent(parentRev, directPrev)

            // Compute limits to guess the preferable merge side
            val prevPrev = prevRev(prev).value
            val leftLimit = boundIdx(prevPrev + 1)
            val middleLimit = boundIdx(directPrev + 1)
            val rightLimit = rightLimitRev(inter).value

            // Left merge
            if (2 * middleLimit >= leftLimit + rightLimit) {
              parentRev(inter).setValue(prev)
              rightLimitRev(prev).setValue(rightLimit)
            }
            // Right merge
            else {
              parentRev(prev).setValue(inter)
              untilCriticalRev(inter).setValue(untilCriticalRev(prev).value)
              prevRev(inter).setValue(prevPrev)
            }
          }
        }
      }
    }

    Suspend
  }

  @inline private def removeUnbound(list: UnboundList, i: Int) {
    import list._
    val prev = prevRev(i).value
    val next = nextRev(i).value

    if (prev == -1) {
      firstRev.setValue(next)
    } else {
      nextRev(prev).setValue(next)
    }
    if (next != nRelevantVariables) {
      prevRev(next).setValue(prev)
    }
  }
  
  @inline private def findParent(parentTable: Array[ReversibleInt], child: Int): Int = {
    var parent = parentTable(child).value
    if (parent == child) return child
    if (parent == -1) return -1
    
    var ancestor = parentTable(parent).value
    if (ancestor == parent) return parent
    
    while (ancestor != parent) {
      parent = ancestor
      ancestor = parentTable(parent).value
    }
    parentTable(child).setValue(parent)
    parent
  }
}