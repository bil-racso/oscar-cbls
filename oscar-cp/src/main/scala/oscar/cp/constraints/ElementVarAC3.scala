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
import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.util.ArrayUtils
import oscar.algo.reversible.ReversibleInt
import scala.math.min
import scala.math.max
import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPSolver
import oscar.algo.reversible.ReversibleInt
import oscar.algo.reversible.ReversibleSparseSet
import oscar.algo.reversible.ReversibleIntWithCache


/**
 * A full Arc-Consistent Element Constraint: y(x) == z based on AC3 only
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert
 */
class ElementVarAC3(y: Array[CPIntVar], x: CPIntVar, z: CPIntVar) extends Constraint(y(0).store, "ACElementVar") {
    
  private[this] val xRange = max(0, x.min) to min(x.max, y.size)
  private[this] val zRange = (z.min max (y.map(_.min).min)) to (z.max min (y.map(_.max).max))
  
  
  private[this] val supporty = Array.fill(y.length)(0)
  private[this] val supportz = Array.fill(zRange.size)(0)
  private[this] val minZ = zRange.min
  
  idempotent = true
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    if (z.updateMax((y.map(_.max).max)) == Failure) return Failure
    if (z.updateMin((y.map(_.min).min)) == Failure) return Failure
    if (x.updateMin(0) == Failure) return Failure
    if (x.updateMax(y.size - 1) == Failure) return Failure

    if (adjustX() == Failure) Failure
    else {

      if (x.isBound) {
        return s.post(new Eq(y(x.min),z))
      } else {
        
        // Replaces this constraint by an Equality constraint.
        val equality = new ElementEq(y, x, z)

        x.filterWhenBind(true,CPStore.MaxPriorityL2) {
          if (s.post(equality) == Failure) Failure
          else {
            deactivate()
            Success 
          }            
        }

        x.callPropagateWhenDomainChanges(this)
        z.callPropagateWhenDomainChanges(this)
        for (i <- x.min to x.max; if x hasValue i) {
          y(i).callPropagateWhenDomainChanges(this)
        }
        propagate()
      }

    }
  }
  
  private[this] val zvalues = Array.ofDim[Int](zRange.size)
   
  @inline private[this] def updateSupporty(i: Int, zvalues: Array[Int], m: Int): Boolean = {
    if (y(i).hasValue(supporty(i)) && z.hasValue(supporty(i))) return true
    else {
      var k = 0
      while (k < m) {
        if (y(i).hasValue(zvalues(k))) {
          supporty(i) = zvalues(k)
          return true
        }
        k += 1
      }
      return false
    }
    
  }
  
  private[this] val xvalues = Array.ofDim[Int](y.length)
  
  @inline private[this] def updateSupportz(v: Int, xvalues: Array[Int], m: Int): Boolean = {
    if (x.hasValue(supportz(v-minZ)) && y(supportz(v-minZ)).hasValue(v)) return true
    else {
      var i = 0
      while (i < m) {
        if (x.hasValue(xvalues(i)) && y(xvalues(i)).hasValue(v)) {
          supportz(v-minZ) = xvalues(i)
          return true
        }
        i += 1
      }
      return false
    }
  }  

  override def propagate(): CPOutcome = {
    
    var mz = z.fillArray(zvalues)
    val mx = x.fillArray(xvalues)

    var i = 0
    while (i < mz) {
      if (!updateSupportz(zvalues(i),xvalues,mx)) {
        if (z.removeValue(zvalues(i)) == Failure) return Failure
        else {
          zvalues(i) = zvalues(mz-1)
          mz -= 1
          i -= 1
        }
      }
      i += 1
    }
    i = 0
    while (i < mx) {
      if (!updateSupporty(xvalues(i),zvalues,mz)) {
        if (x.removeValue(xvalues(i)) == Failure) return Failure
      }
      i += 1
    }
    Suspend
  }
  
  // Removes each value i in x that is not a valid id in y
  @inline private def adjustX(): CPOutcome = {
    if (x.updateMin(0) == Failure) Failure
    else if (x.updateMax(y.size - 1) == Failure) Failure
    else if (x.isBound) valBind(x)
    else Suspend
  }

  private class ElementEq(ys: Array[CPIntVar], x: CPIntVar, z: CPIntVar) extends Constraint(x.store, "ElementEq") {

    // Used to iterate on the domain of the variables
    private[this] val values = new Array[Int](ys.map(_.size).max max x.size max z.size)

    private[this] var y: CPIntVar = null

    final override def setup(l: CPPropagStrength): CPOutcome = {
      y = ys(x.min)
      if (propagate() == Failure) Failure
      else {
        y.callValRemoveWhenValueIsRemoved(this)
        z.callValRemoveWhenValueIsRemoved(this)
        Suspend
      }
    }

    final override def propagate(): CPOutcome = {
      var i = y.fillArray(values)
      while (i > 0) {
        i -= 1
        val value = values(i)
        if (!z.hasValue(value) && y.removeValue(value) == Failure) return Failure
      }
      i = z.fillArray(values)
      while (i > 0) {
        i -= 1
        val value = values(i)
        if (!y.hasValue(value) && z.removeValue(value) == Failure) return Failure
      }
      Suspend
    }

    // FIXME: should be idempotent (not allowed yet for L1 events)
    final override def valRemove(intVar: CPIntVar, value: Int): CPOutcome = {
      if (intVar == y) z.removeValue(value)
      else y.removeValue(value)
    }
  }

}






