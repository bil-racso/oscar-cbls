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

import oscar.cp.modeling.TightenType
import oscar.cp.modeling.TightenType._
import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.search.Objective
import oscar.cp.core.CPPropagStrength

/**
 * @author Pierre Schaus  pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
abstract class CPObjectiveUnit(val objVar: CPVarInt, val n: String = "") extends Objective {
  
  // Upper bound of the objective
  protected val lb = objVar.min - 1 // Avoids to remove the min value in the first propagate
  // Lower bound of the objective
  protected val ub = objVar.max + 1 // Avoids to remove the max value in the first propagate
  // Tightening mode of the objective
  protected var tightenType = StrongTighten
  // Best so far value of the objective (the one recorded on the last tighten)
  var best = 0
  
  /** Returns the best value in the current domain of the objective */
  def domBest: Int
  /** Returns the worst value in the current domain of the objective */
  def domWorst: Int
  /** Returns true if the objective has to be maximized, false otherwise */
  def isMax: Boolean
  /** Returns true if the objective has to be minimized, false otherwise */
  def isMin: Boolean
  /** Tries to adjust the worst bound of the domain to newBound with delta */
  def updateWorstBound(newBound: Int, delta: Int = 0): CPOutcome
  /** Tries to adjust the best bound of the domain to newBound with delta */
  def updateBestBound(newBound: Int, delta: Int = 0): CPOutcome
  /** Returns the value of the worst bound of the objective */
  def worstBound: Int
  /** Returns the value of the best bound of the objective */
  def bestBound: Int

  /** Sets the tightening mode of the objective */
  def tightenMode_=(t: TightenType.Value) = {
    tightenType = t
  }
  
  /** Returns the tightening mode of the objective */
  def tightenMode = tightenType
  
  /** Tightens the objective according to its tightening mode */
  def tighten(): Unit = {
    if (!objVar.isBound) throw new RuntimeException("objective"+n+" not bound:" + objVar)
    else {
      best = objVar.value // Sets new best value
      if (!objVar.store.silent && tightenType != NoTighten) {
        println("objective"+n+" tightened to " + best + " lb:"+  lb)
      }
    }
  }

  /** Returns true if the objective has reached is optimal value, false otherwise */
  def isOptimum: Boolean = (best == bestBound)

  /** Returns true if the objective is consistent according to its model */
  def isOK(): Boolean = ensureBest() != CPOutcome.Failure
  
  /** Restores the lower and upper bounds of the objective as well as its best so far value */
  def relax(): Unit = {
    best = worstBound
  }
  
  /** Adjusts the bounds of the objective according to the best so far value and to the 
   *  tightening mode */
  def ensureBest(): CPOutcome = {
    if (tightenType == NoTighten) Suspend
    else {
      val delta = if (tightenType == StrongTighten) 1 else 0
      val oc = updateWorstBound(best, delta)
      //println("ensure best:"+oc+" best:"+best+" objVar:"+objVar)
      oc
    }
  }
  
  override def toString: String = "best value: "+best+" tightening: "+tightenType
}

/** Best  : smallest value
 *  Worst : largest value
 */
class CPObjectiveUnitMinimize(objVar: CPVarInt, n: String = "") extends CPObjectiveUnit(objVar, n) {

  def domBest: Int = objVar.min
  def domWorst: Int = objVar.max 
  def isMax: Boolean = false
  def isMin: Boolean = true 
  def updateWorstBound(newBound: Int, delta: Int = 0): CPOutcome = {
    objVar.updateMax(newBound-delta) 
  }
  def updateBestBound(newBound: Int, delta: Int = 0): CPOutcome = objVar.updateMin(newBound+delta)
  def worstBound: Int = ub
  def bestBound: Int = lb
  
  // Init best
  best = ub
}

/** Best  : largest value
 *  Worst : smallest value
 */
class CPObjectiveUnitMaximize(objVar: CPVarInt, n: String = "") extends CPObjectiveUnit(objVar, n) {

  def domBest: Int = objVar.max
  def domWorst: Int = objVar.min 
  def isMax: Boolean = true
  def isMin: Boolean = false
  def updateWorstBound(newBound: Int, delta: Int = 0): CPOutcome = objVar.updateMin(newBound+delta)   
  def updateBestBound(newBound: Int, delta: Int = 0): CPOutcome = objVar.updateMax(newBound-delta) 
  def worstBound: Int = lb
  def bestBound: Int = ub
  
  // Init best
  best = lb
}
