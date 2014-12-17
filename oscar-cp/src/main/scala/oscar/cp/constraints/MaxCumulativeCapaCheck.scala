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
 * *****************************************************************************/
package oscar.cp.constraints

import scala.math.max
import scala.math.min
import oscar.cp.core.CPStore
import oscar.cp.core.CPIntVar
import oscar.cp.core.CPIntervalVar
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling.CPSolver
import oscar.algo.SortUtils.stableSort

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class MaxCumulativeCapaCheck(starts: Array[_ <: CPIntervalVar], durations: Array[_ <: CPIntervalVar], ends: Array[_ <: CPIntervalVar], demands: Array[_ <: CPIntervalVar], resources: Array[CPIntVar], capacity: CPIntervalVar, id: Int) extends Constraint(capacity.store, "MaxSweepCumulative") {

  private val nTasks = starts.size
  private val Tasks = 0 until nTasks

  override def setup(l: CPPropagStrength): CPOutcome = {
    capacity.callPropagateWhenBoundsChange(this)    
    return propagate()
  }
  
  override def propagate(): CPOutcome = {
    for (i <- Tasks; if durations(i).min > 0) {
      if (demands(i).min > capacity.max) {
        if (resources(i).removeValue(id) == CPOutcome.Failure) return CPOutcome.Failure
      } 
    }
    if (Tasks.forall(i => durations(i).max == 0 || demands(i).max < capacity.min || !resources(i).hasValue(id))) CPOutcome.Success
    else CPOutcome.Suspend
  }
}
  

