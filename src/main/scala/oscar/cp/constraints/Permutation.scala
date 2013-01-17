/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

package oscar.cp.constraints;

import oscar.cp.modeling._
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.reversible.ReversibleInt
import scala.math.min
import scala.math.max
import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.modeling.CPSolver
import oscar.reversible.ReversibleInt
import oscar.reversible.ReversibleSetIndexedArray
import java.security.InvalidParameterException


/**
 * n = x.size-1 = y.size-1
 * x is a permutation of {0 ... n-1} and y also
 * x and y are such that x(y(i)) = i i.e. y(i) is the position of number i in x
 * @author Pierre Schaus - pschaus@gmail.com
 */
class Permutation(x: Array[CPVarInt], y: Array[CPVarInt]) extends Constraint(y(0).s, "Permutation") {
    
  val n = x.size-1
  if (x.size != y.size) throw new InvalidParameterException("x and y must have the same size")
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    
    for (i <- 0 to n) {
      if (x(i).updateMin(0) == Failure) return Failure
      if (y(i).updateMin(0) == Failure) return Failure
      if (x(i).updateMax(n) == Failure) return Failure
      if (y(i).updateMax(n) == Failure) return Failure
    }
    if (s.post(allDifferent(x),l) == Failure) return Failure
    if (s.post(allDifferent(y),l) == Failure) return Failure
    
    for(i <- 0 to n; v <- 0 to n) {
      if (!x(i).hasValue(v)) {
        if (y(v).removeValue(i) == Failure) return Failure
      }
      if (!y(i).hasValue(v)) {
        if (x(v).removeValue(i) == Failure) return Failure
      }
    }
    for(i <- 0 to n) {
      x(i).callValRemoveIdxWhenValueIsRemoved(this, i)
      y(i).callValRemoveIdxWhenValueIsRemoved(this, n+1+i)
    }
    Suspend
  }


  override def valRemoveIdx(cpvar: CPVarInt, i: Int, v: Int): CPOutcome = {
    if (i <= n) {
      // x(i) lost the value v
      y(v).removeValue(i)
    } else {
      // y(i-n-1) lost the value v
      x(v).removeValue(i-n-1)
    }
  }


}