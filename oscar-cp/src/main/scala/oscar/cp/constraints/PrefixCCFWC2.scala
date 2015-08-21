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
 * @author Victor Lecomte
 */
class PrefixCCFWC2(X: Array[CPIntVar], minVal: Int, lowerLists: Array[Array[(Int, Int)]], upperLists: Array[Array[(Int, Int)]])
  extends Constraint(X(0).store, "PrefixCCFWC2") {

  private[this] val nVariables = X.length
  private[this] val nValues = lowerLists.length
  private[this] val maxVal = minVal + nValues - 1

  private[this] var changeBuffer: Array[Int] = null

  private[this] val lower = Array.tabulate(nValues, nVariables + 1)((vi, i) => 0)
  private[this] val upper = Array.tabulate(nValues, nVariables + 1)((vi, i) => i)

  private[this] val lowerDiffRev = Array.ofDim[ReversibleInt](nValues, nVariables)
  private[this] val upperDiffRev = Array.ofDim[ReversibleInt](nValues, nVariables)

  private[this] val gapFenwick = Array.ofDim[ReversibleInt](nValues, nVariables + 1)

  private[this] val prevRev = Array.ofDim[ReversibleInt](nValues, nVariables)
  private[this] val nextRev = Array.ofDim[ReversibleInt](nValues, nVariables)

  private[this] var nHazardsRev: ReversibleInt = null
  private[this] var nHazardsTmp = 0

  /*private def toChar(vi: Int, i: Int): Char = {
    (lowerDiffRev(vi)(i).value, upperDiffRev(vi)(i).value) match {
      case (0, 0) => '-'
      case (1, 1) => '/'
      case (0, 1) => '<'
      case (1, 0) => '>'
    }
  }

  private def printLine(vi: Int, brackets: Boolean, show: Boolean): Unit = {
    if (brackets) print("[")
    if (show) print("(")
    var next = -1
    for (i <- 0 until nVariables) {
      if (!(brackets || show) || i != 0) print(" ")
      val c = toChar(vi, i)
      if (c == '<' && next == -1)
        next = i
      if (i == next) {
        print(c)
        next = nextRev(vi)(i).value
      } else if (show) {
        print(c)
      } else {
        print(" ")
      }
    }
    if (brackets) print("]")
    if (show) print(")")
    println()
  }

  private def printBar(): Unit = {
    for (i <- 0 until nVariables)
      print("--")
    println("-")
  }

  private def printStatus(thisVi: Int): Unit = {
    printBar()
    for (vi <- 0 until nValues) {
      if (vi == thisVi) printLine(vi, brackets = true, show = false)
      else printLine(vi, brackets = false, show = false)
    }
    printBar()
    printLine(thisVi, brackets = false, show = true)
    printBar()
  }*/

  override def setup(l: CPPropagStrength): CPOutcome = {

    val feasibleLower = (index: Int, value: Int) => value <= index
    val feasibleUpper = (index: Int, value: Int) => value >= 0
    val keepMin = (i: Int, j: Int) => i min j
    val keepMax = (i: Int, j: Int) => i max j
    val flat = (value: Int) => value
    val slopeForwards = (value: Int) => value + 1
    val slopeBackwards = (value: Int) => value - 1

    if (readArguments(lower, lowerLists, feasibleLower, keepMax) == Failure)
      return Failure
    if (readArguments(upper, upperLists, feasibleUpper, keepMin) == Failure)
      return Failure

    cleverFill(lower, flat, slopeBackwards, keepMax)
    cleverFill(upper, slopeForwards, flat, keepMin)

    //println("before test")

    if (testAndDeduceBetweenValues() == Failure)
      return Failure

    cleverFill(lower, flat, slopeBackwards, keepMax)
    cleverFill(upper, slopeForwards, flat, keepMin)

    /*println("filled:")
    for (vi <- 0 until nValues) {
      for (bound <- Seq(lower(vi), upper(vi))) {
        println(s"$vi: ${bound mkString " "}")
      }
    }*/

    registerForChanges()

    initAndPrune()
  }

  private def readArguments(bounds: Array[Array[Int]], boundLists: Array[Array[(Int, Int)]],
                            feasible: (Int, Int) => Boolean, keep: (Int, Int) => Int): CPOutcome = {
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

        bounds(vi)(index) = keep(bounds(vi)(index), value)
      }
    }

    Suspend
  }

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

  private def testAndDeduceBetweenValues(): CPOutcome = {
    // Perform some deduction based on other values' lower and upper bounds
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

  private def initAndPrune(): CPOutcome = {
    implicit val store = s

    val unboundTmp = Array.ofDim[Boolean](nValues, nVariables)
    var vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = vi + minVal
      var i = nVariables
      while (i > 0) {
        i -= 1
        unboundTmp(vi)(i) = X(i).hasValue(v) && !X(i).isBoundTo(v)
      }
    }

    vi = nValues
    while (vi > 0) {
      vi -= 1
      val v = vi + minVal

      var lastUnbound = -1
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

          // If there are flats or slopes with no gap
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

    Success
  }

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

        //println(s"removed value $vi from $i")
        //printStatus(vi)

        val lowerDiff = lowerDiffRev(vi)(i).value
        val upperDiff = upperDiffRev(vi)(i).value
        val fenwick = gapFenwick(vi)

        var gapLeft = 0
        var gapRight = 0
        if (lowerDiff == 1 || upperDiff == 1) {
          gapLeft = getGap(fenwick, i)
          gapRight = gapLeft + upperDiff - lowerDiff
        }

        if (lowerDiff == 1) {
          lowerDiffRev(vi)(i).setValue(0)
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

        if (upperDiff == 1) {
          upperDiffRev(vi)(i).setValue(0)
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

        removeUnbound(vi, i)
        //printStatus(vi)
      }
    }

    // Treat the assignments
    if (x.isBound) {
      val v = x.min
      // If the value removed is one we track
      if (minVal <= v && v <= maxVal) {
        val vi = v - minVal

        //println(s"assigned value $vi to $i")
        //printStatus(vi)

        val lowerDiff = lowerDiffRev(vi)(i).value
        val upperDiff = upperDiffRev(vi)(i).value
        val fenwick = gapFenwick(vi)

        var gapLeft = 0
        var gapRight = 0
        if (lowerDiff == 0 || upperDiff == 0) {
          gapLeft = getGap(fenwick, i)
          gapRight = gapLeft + upperDiff - lowerDiff
        }

        if (lowerDiff == 0) {
          lowerDiffRev(vi)(i).setValue(1)
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

        if (upperDiff == 0) {
          upperDiffRev(vi)(i).setValue(1)
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

        removeUnbound(vi, i)
        //printStatus(vi)
      }
    }

    if (nHazardsTmp != nHazardsOld)
      nHazardsRev.setValue(nHazardsTmp)

    Suspend
  }

  @inline private def getGap(fenwick: Array[ReversibleInt], i: Int): Int = {
    var sum = 0
    var cur = i
    while (cur > 0) {
      sum += fenwick(cur).value
      cur -= (cur & -cur)
    }
    sum
  }

  @inline private def incrGap(fenwick: Array[ReversibleInt], i: Int) {
    var cur = i
    while (cur <= nVariables) {
      fenwick(cur).incr()
      cur += (cur & -cur)
    }
  }

  @inline private def decrGap(fenwick: Array[ReversibleInt], i: Int) {
    var cur = i
    while (cur <= nVariables) {
      fenwick(cur).decr()
      cur += (cur & -cur)
    }
  }

  @inline private def removeUnbound(vi: Int, i: Int): Unit = {
    val prev = prevRev(vi)(i).value
    val next = nextRev(vi)(i).value
    if (prev != -1)
      nextRev(vi)(prev).setValue(next)
    if (next != nVariables)
      prevRev(vi)(next).setValue(prev)
  }
}