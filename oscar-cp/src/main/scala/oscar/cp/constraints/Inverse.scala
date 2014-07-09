/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.constraints

import oscar.cp.core.CPIntVar
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/**
 * Inverse
 *
 *  This constraint enforces the following rules:
 *  1. prev(next(i)) == i
 *  2. next(prev(i)) == i
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

class Inverse(prev: Array[CPIntVar], next: Array[CPIntVar]) extends Constraint(prev.head.store, "Inverse") {

  // Checks the consistency of the arguments
  require(prev.length == next.length, "input arrays must have the same size")

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (init() == Failure) Failure
    else {
      var i = 0
      while (i < prev.length) {
        if (!prev(i).isBound) prev(i).callValRemoveIdxWhenValueIsRemoved(this, i)
        if (!next(i).isBound) next(i).callValRemoveIdxWhenValueIsRemoved(this, i)
        i += 1
      }
      Suspend
    }
  }

  @inline
  private def init(): CPOutcome = {
    var i = 0
    while (i < prev.length) {
      // Initializes the bounds of the variables
      if (!initBounds(prev(i))) return Failure
      else if (!initBounds(next(i))) return Failure
      else {
        var j = 0
        while (j < prev.length) {
          // Initializes inner domains
          if (!init(prev, next, i, j)) return Failure
          else if (!init(next, prev, i, j)) return Failure
          else j += 1
        }
      }
      i += 1
    }
    Suspend
  }

  @inline
  private def initBounds(intVar: CPIntVar): Boolean = {
    if (intVar.updateMin(0) == Failure) false
    else if (intVar.updateMax(prev.length - 1) == Failure) false
    else true
  }

  @inline
  private def init(vector1: Array[CPIntVar], vector2: Array[CPIntVar], i: Int, j: Int): Boolean = {
    if (!vector1(i).hasValue(j)) true
    else if (vector1(i).isBound) vector2(j).assign(i) != Failure
    else if (!vector2(j).hasValue(i)) vector2(i).removeValue(j) != Failure
    else true
  }

  override def valRemoveIdx(intVar: CPIntVar, id: Int, value: Int): CPOutcome = {
    if (intVar == next(id)) prev(value).removeValue(id)
    else next(value).removeValue(id)
  }
}

