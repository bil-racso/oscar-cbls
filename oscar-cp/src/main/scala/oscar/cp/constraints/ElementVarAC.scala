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
 * A full Arc-Consistent Element Constraint: y(x) == z
 *
 * @author Renaud Hartert ren.hartert@gmail.com, 
 * @author Pierre Schaus pschaus@gmail.com
 */
class ElementVarAC(y: Array[CPIntVar], x: CPIntVar, z: CPIntVar) extends Constraint(y(0).store, "ACElementVar") {
    
  private[this] val xRange = max(0, x.min) to min(x.max, y.size)
  private[this] val zRange = (z.min max (y.map(_.min).min)) to (z.max min (y.map(_.max).max))
  
  // Number of supports for the value v i.e number of indices i such that v is in y(i)
  private[this] val _nSupports = Array.fill(zRange.size)( new ReversibleIntWithCache(s, 0, y.size+1) ) //// for some reasons, does not work with cache
  
  // For all indices i in x: intersect(i) is the size of the intersection between y(i) and z
  private[this] val _intersect = Array.fill(xRange.size)(new ReversibleSparseSet(s, z.min, z.max))
  
  // Mapping functions used to limit the size of both previous structures
  private[this] def nSupports(i: Int) = _nSupports(i-zRange.min)
  private[this] def intersect(i: Int) = _intersect(i-xRange.min)
  
  private[this] val values = Array.ofDim[Int](y.map(_.size).max max x.size max z.size)

  override def setup(l: CPPropagStrength): CPOutcome = {
    if (z.updateMax((y.map(_.max).max)) == Failure) return Failure
    if (z.updateMin((y.map(_.min).min)) == Failure) return Failure
    if (x.updateMin(0) == Failure) return Failure
    if (x.updateMax(y.size-1) == Failure) return Failure
    
    if (adjustX() == Failure) Failure
    else {
      val out = propagateInitial()
      if (out != Suspend) out
      else {
        x.callValRemoveWhenValueIsRemoved(this)
        z.callValRemoveWhenValueIsRemoved(this)
        for (i <- x.min to x.max; if x hasValue i) {
          y(i).callValRemoveIdxWhenValueIsRemoved(this, i)
        }
        Suspend
      }
    }
  }

  def propagateInitial(): CPOutcome = {
    //resetData() // Mandatory if propagate is called after the initial call
    initData()
    
    var i = 0
    var m = x.fillArray(values)
    while (i < m) {
      val v = values(i)
      if (intersect(v).size == 0) {
        if (x.removeValue(v) == Failure) {
          return Failure
        }
      }
      i += 1
    }
    
    if (x.isBound) return bindX()
    i = 0
    m = z.fillArray(values)
    while (i < m) {
      val v = values(i)
      if (nSupports(v).value == 0) {
        if (z.removeValue(v) == Failure) {
          return Failure
        }
      }      
      i += 1
    }    
    Suspend
  }

  override def valRemoveIdx(cpvar: CPIntVar, i: Int, v: Int): CPOutcome = {
    removeFromY(i, v)
  }

  override def valRemove(cpvar: CPIntVar, v: Int): CPOutcome = {
    if (cpvar == x) removeFromX(v)
    else removeFromZ(v)
  }

  // Initializes data structures
  private def initData() {
    for (i <- x) {
      val keep =
        (for (v <- y(i); if (z.hasValue(v))) yield {
          nSupports(v).incr()
          v 
        }).toSet
      for (v <- intersect(i).min to intersect(i).max; if !(keep.contains(v))) {
        intersect(i).removeValue(v)
      }
    }
  }
  
  // Reset the content of both data structures
  private def resetData() {
    for (i <- 0 until _intersect.size)
      _intersect(i).empty()
    for (v <- 0 until _nSupports.size)
      _nSupports(v).setValue(0)
  }

  // Reduces the number of supports of the value v
  @inline private def reduceSupports(v: Int): CPOutcome = {
    if (zRange.contains(v) && nSupports(v).value >= 1 && nSupports(v).decr() == 0) {
      z.removeValue(v) 
    }
    else Suspend
  }

  // Removes the value v from the intersection between y(i) and z
  @inline private def reduceIntersect(i: Int, v: Int): CPOutcome = {
    intersect(i).removeValue(v)
    if (intersect(i).isEmpty) {
      x.removeValue(i) 
    }
    else Suspend
  }

  // Removes v from all the intersections
  @inline private def removeFromZ(v: Int): CPOutcome = { 
    nSupports(v).value = 0
    var i = 0
    val m = x.fillArray(values)
    while (i < m) {
      if (reduceIntersect(values(i), v) == Failure) return Failure
      i += 1
    }
    Suspend
  }

  // If x is bound, this constraint is replaced by an Equality constraint
  // else, the number of supports for all values v in y(i) is reduced by 1
  @inline private def removeFromX(i: Int): CPOutcome = {
    if (x.isBound) bindX()
    else {
      val m = y(i).fillArray(values)
      var k = 0
      while (k < m) {
        if (values(k) < zRange.max) {
          if (reduceSupports(values(k)) == Failure) return Failure
        }
        k += 1
      }
      Suspend
    }
  }

  // If y(i) has an intersection with z, the number of supports of the  value v is reduced by 1
  @inline private def removeFromY(i: Int, v: Int): CPOutcome = {
    // we must check that x has value to avoid reducing twice for the same removal
    // y(i) might loose the value and i is also removed ...
    if (x.hasValue(i) && reduceSupports(v) == Failure) Failure 
    else reduceIntersect(i, v)
  }

  // Replaces this constraint by an Equality constraint
  private[this] val eqCons = y.map(new Eq(_,z))
  @inline private def bindX(): CPOutcome = {
    if (s.post(eqCons(x.min)) == Failure) Failure
    else Success
  }

  // Removes each value i in x that is not a valid id in y
  @inline private def adjustX(): CPOutcome = {
    if (x.updateMin(0) == Failure) Failure
    else if (x.updateMax(y.size - 1) == Failure) Failure
    else if (x.isBound) bindX()
    else Suspend
  }
}
