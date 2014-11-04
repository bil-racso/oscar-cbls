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

import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.CPOutcome._

/**
 * Implementation of AllDifferent
 * @author Pierre Schaus pschaus@gmail.com
 */
class AllDiffFWC(val X: Array[CPIntVar]) extends Constraint(X(0).store, "AllDiffFWC") {

  val x = X.map(i => i)
  val nBounds = new ReversibleInt(s, 0)

  override def setup(l: CPPropagStrength): CPOutcome = {
    priorityL2 = CPStore.MaxPriorityL2 - 1
    X.foreach(_.callPropagateWhenBind(this))
    val oc = propagate()
    oc
  }

  private def setBound(i: Int) {
    val tmp = x(nBounds.value)
    x(nBounds.value) = x(i)
    x(i) = tmp
    nBounds.incr()
  }
  


  override def propagate(): CPOutcome = {

    var i = nBounds.value
    while (i < x.size) {
      if (x(i).isBound) {
        val v = x(i).value
        var j = nBounds.value
        while (j < x.size) {
          if (j != i && x(j).removeValue(v) == Failure) return Failure
          j += 1
        }
        setBound(i)
      }
      i += 1
    }
    Suspend
  }

}


