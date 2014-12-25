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

package oscar.cp.constraints;

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils;
import oscar.algo.reversible.ReversibleInt

import scala.math.min
import scala.math.max

import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPSolver
import oscar.algo.reversible.ReversibleInt


/**
 * Bound Consistent Element Constraint: y(x) == z
 *
 * @author Pierre Schaus - pschaus@gmail.com
 */
class ElementVarBC(val y: Array[CPIntVar], val x: CPIntVar, val z: CPIntVar) extends Constraint(y(0).store, "BCElementVar") {
    
  private val xRange = max(0, x.min) to min(x.max, y.size)
  private val zRange = (z.min max (y.map(_.min).min)) to (z.max min (y.map(_.max).max))
  

  private val zminSup = new ReversibleInt(s, 0)
  private val zmaxSup = new ReversibleInt(s, 0)


  override def setup(l: CPPropagStrength): CPOutcome = {
    if (z.updateMax((y.map(_.max).max)) == Failure) return Failure
    if (z.updateMin((y.map(_.min).min)) == Failure) return Failure
    if (x.updateMin(0) == Failure) return Failure
    if (x.updateMax(y.size-1) == Failure) return Failure
    
    updateSupport()
    if (filterZ() == Failure) return Failure
    for (i <- x) {
      y(i).callUpdateBoundsIdxWhenBoundsChange(this, i)
    }
    x.callValRemoveWhenValueIsRemoved(this)
    x.callValBindWhenBind(this)
    z.callUpdateBoundsWhenBoundsChange(this)
    Suspend
    
  }
  
  def filterZ() = {
	if (z.updateMin(y(zminSup.value).min) == Failure) Failure
	else if (z.updateMax(y(zmaxSup.value).max) == Failure) Failure
	else Suspend
  }
  
  def filterX(): CPOutcome = {
	
    val toRemove = x.filter(i => y(i).max < z.min || y(i).min > z.max)
	for (v <- toRemove) {
	  if (x.removeValue(v) == Failure) return Failure
	}
	
	/*  
	var i = x.min	
	while (i <= x.max) {
	  if (x.hasValue(i) && y(i).max < z.min || y(i).min > z.max) {
	    if (x.removeValue(i) == Failure) return Failure
	  }
	  i += 1
	}*/
	
	Suspend
  }  
  
  override def updateBounds(cpvar: CPIntervalVar): CPOutcome = {
    // bounds of z changed
    if (filterX() == Failure) Failure
    else if (x.isBound) valBind(x)
    else Suspend
  }
  
  override def valBind(cpvar: CPIntervalVar): CPOutcome = {
    // x is bind
    val i = x.min
    zminSup.setValue(i)
    zmaxSup.setValue(i)
    if (y(i).updateMax(z.max) == Failure) Failure
    else if (y(i).updateMin(z.min) == Failure) Failure
    else filterZ()
  }

  override def updateBoundsIdx(cpvar: CPIntervalVar, i: Int): CPOutcome = {
    // bound of y(i) changed
    if (y(i).max < z.min || y(i).min > z.max) {
      if (x.removeValue(i) == Failure) return Failure
    }
    // bound of y(i) has changed, if i was a supper, we must update the supports
    if (zminSup.value == i || zmaxSup.value == i) {
      updateSupport()
      filterZ()
    } else {
      Suspend
    }
  }
  
  override def valRemove(cpvar: CPIntVar, v: Int): CPOutcome = {
    // x lost value v
    if (zminSup.value == v || zmaxSup.value == v) {
      updateSupport()
      filterZ()
    } else {
      Suspend
    }
  }
  
  def updateSupport() {
    var supmin = 0
    var supmax = 0
    var min = Int.MaxValue
    var max = Int.MinValue
    val ite = x.iterator
    while (ite.hasNext) {
      val v = ite.next
      if (y(v).min < min) {
        min = y(v).min
        supmin = v
      }
      if (y(v).max > max) {
        max = y(v).max
        supmax = v
      }
    }
    zminSup.setValue(supmin)
    zmaxSup.setValue(supmax)
  }

}
