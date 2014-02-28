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
 * Implementation of Sum Constraint:
 * @author Pierre Schaus pschaus@gmail.com
 */
class Sum(val X: Array[CPIntVar], val y: CPIntVar) extends Constraint(y.s, "Sum2") {

  val x = X.map(i => i)
  val sumBounds = new ReversibleInt(s, 0)
  val nBounds = new ReversibleInt(s, 0)

  override def setup(l: CPPropagStrength): CPOutcome = {
    priorityL2 = CPStore.MAXPRIORL2 - 1
    X.foreach(_.callPropagateWhenBoundsChange(this, false))
    y.callPropagateWhenBoundsChange(this, false)
    val oc = propagate()
    //println("oc:"+oc)
    oc
  }

  private def setBound(i: Int) {
    sumBounds.value = sumBounds.value + x(i).value
    val tmp = x(nBounds.value)
    x(nBounds.value) = x(i)
    x(i) = tmp
    nBounds.incr()
  }

  override def propagate(): CPOutcome = {

    var ymin = sumBounds.value
    var ymax = sumBounds.value
    var i = nBounds.value
    while (i < x.size) {
      ymin += x(i).min
      ymax += x(i).max
      if (x(i).isBound) {
        setBound(i)
      }
      i += 1
    }

    if (y.updateMax(ymax) == Failure) {
      return Failure
    }
    if (y.updateMin(ymin) == Failure) {
      return Failure
    }

    if (y.max == ymax && y.min == ymin) {
      return Suspend
    }

    var pruneMin = y.min != ymin
    val pruneMax = y.max != ymax

    // prune max
    if (pruneMax) {
      i = nBounds.value
      while (i < x.size) {
        if (!x(i).isBound) {
          val ymini = ymin - x(i).min
          val ximax = y.max - ymini
          val oc = x(i).updateMax(ximax) // cannot fail
          assert(oc != Failure)
        } else setBound(i)
        i += 1
      }
    }
    // prune min
    if (pruneMin) {
      i = nBounds.value
      while (i < x.size) {
        if (!x(i).isBound) {
          val ymaxi = ymax - x(i).max
          val ximin = y.min - ymaxi
          val oc = x(i).updateMin(ximin) // cannot fail
          assert(oc != Failure)
        } else setBound(i)
        i += 1
      }
    }
    Suspend
  }

}


