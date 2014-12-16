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
import math.max

/**
 * Implementation of Sum Constraint:
 * @author Pierre Schaus pschaus@gmail.com
 * @author Steven Gay steven.gay@uclouvain.be
 */
class Sum(val X: Array[_ <: CPIntervalVar], val y: CPIntervalVar) extends Constraint(y.store, "Sum") {

  val x = X.map(i => i.asInstanceOf[CPIntervalVar])
  val xSize = x.size
  val sumBoundVars = new ReversibleInt(s, 0)
  val nBoundVars = new ReversibleInt(s, 0)
  
  idempotent = true

  override def setup(l: CPPropagStrength): CPOutcome = {
    
    priorityL2 = CPStore.MAXPRIORL2 - 1
    X.foreach(_.callPropagateWhenBoundsChange(this))
    y.callPropagateWhenBoundsChange(this)
    val oc = propagate()
    //println("oc:"+oc)
    oc
  }

  private def setBound(i: Int) {
    sumBoundVars.value = sumBoundVars.value + x(i).min
    val tmp = x(nBoundVars.value)
    x(nBoundVars.value) = x(i)
    x(i) = tmp
    nBoundVars.incr()
  }

  override def propagate(): CPOutcome = {
    // invariant to maintain: sumBoundVars is the sum of variables x(0 until nBounds), which have to be bound.

    var sumxmin = 0 
    var sumxmax = 0
    var maxDiff = 0
    var reduce = true
    var i = 0
    
    while(reduce) {
      reduce = false
      sumxmin = sumBoundVars.value
      sumxmax = sumBoundVars.value

      // step 1: filter bound variables and get maximum range size 
      var i = nBoundVars.value
      while (i < xSize) {
        val ximax = x(i).max
        val ximin = x(i).min
        
        sumxmin += ximin
        sumxmax += ximax
        maxDiff = max(ximax - ximin, maxDiff)
        
        if (ximax == ximin) setBound(i)
        i += 1
      }
      
      
      // step 2: propagate from x to y
      if (y.updateMax(sumxmax) == Failure) return Failure
      if (y.updateMin(sumxmin) == Failure) return Failure

      
      // step 3: propagate from y to x: if there is a ximax that makes the sum go over y, find and trim it
      val ymax = y.max
      if (sumxmin + maxDiff > ymax) {
        i = nBoundVars.value
        while (i < xSize) {
          // Compute new upper bound for xi
          val oldximax = x(i).max
          val ymini = sumxmin - x(i).min
          val ximax = ymax - ymini

          // Update and check whether something changed
          if (ximax < oldximax) {
            if (x(i).updateMax(ximax) == Failure) return Failure
            
            val newximax = x(i).max
            val xidiff = newximax - oldximax
            sumxmax += xidiff
            if (newximax < ximax) reduce = true   // this can happen when x(i) is a multiplicative or non-bijective view ; then do fixpoint here.
          }
          i += 1
        }
      }
      
      // step 4: same for ximin
      val ymin = y.min
      if (sumxmax - maxDiff < ymin) {
        i = nBoundVars.value
        while (i < xSize) {
          val oldximin = x(i).min
          val ymaxi = sumxmax - x(i).max
          val ximin = ymin - ymaxi
          
          if (ximin > oldximin) {
            if (x(i).updateMin(ximin) == Failure) return Failure
            
            val newxmin = x(i).min
//            val xidiff = newxmin - oldximin
//            sumxmin += xidiff
            if (newxmin > ximin) reduce = true
          }
          i += 1
        }
      }
      
  }
    
  Suspend
  }

}




















