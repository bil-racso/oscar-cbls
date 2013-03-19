/**
 * ***********************************************************************
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
abstract class CPObjectiveUnit(val objVar: CPVarInt, val n: String = "") extends Constraint(objVar.store, "objective"+n) with Objective {
  
  // Upper bound of the objective
  protected var lb = Int.MinValue
  // Lower bound of the objective
  protected var ub = Int.MaxValue
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
  /** Adjusts the bound of the domain to the best so far value with delta */
  def updateToBest(best: Int, delta: Int = 0): CPOutcome
  /** Returns the value of the best bound of the objective */
  def bestBound: Int

  /** Sets the tightening mode of the objective */
  def tightenMode_=(t: TightenType.Value) = {
    tightenType = t
  }
  
  /** Returns the tightening mode of the objective */
  def tightenMode = tightenType
  
  /** Tightens the objective according to its tightening mode */
  def tighten() = {
    if (!objVar.isBound) throw new RuntimeException(name+" not bound:" + objVar)
    else {
      best = objVar.value // Sets new best value
      if (!s.silent && tightenType != NoTighten) {
        println(name+" tightened to " + best + " lb:"+  lb)
      }
    }
  }

  /** Returns true if the objective has reached is optimal value, false otherwise */
  def isOptimum = (best == bestBound)

  /** Returns true if the objective is consistent according to its model */
  def isOK() = s.propagate(this) != CPOutcome.Failure
  
  /** Restores the lower and upper bounds of the objective as well as its best so far value */
  def relax() {
    lb = Int.MinValue
    ub = Int.MaxValue
    best = bestBound
  }
  
  /** Adjusts the bounds of the objective according to the best so far value and to the 
   *  tightening mode */
  override def propagate(): CPOutcome = {
    if (tightenType == NoTighten) Suspend
    else {
      val delta = if (tightenType == StrongTighten) 1 else 0
      if (updateToBest(best, delta) == Failure) Failure
      else Suspend
    }
  }

  def restoreBest() = {} // ?
  
  /** Calls propagate() */
  def filter() = propagate()

  /** Initializes the objective */
  override def setup(l: CPPropagStrength): CPOutcome = {
    lb = objVar.min
    ub = objVar.max
    objVar.callPropagateWhenBoundsChange(this)
    propagate()
  }
  
  override def toString = "best value: "+best+" tightening: "+tightenType
}

class CPObjectiveUnitMinimize(objVar: CPVarInt,n: String = "") extends CPObjectiveUnit(objVar,n) {

  def domBest: Int = objVar.min
  def domWorst: Int = objVar.max 
  def isMax: Boolean = false
  def isMin: Boolean = true 
  def updateToBest(best: Int, delta: Int = 0): CPOutcome = objVar.updateMax(best-delta) 
  def bestBound: Int = lb
  
  // Init best
  best = Int.MaxValue
}

class CPObjectiveUnitMaximize(objVar: CPVarInt, n: String = "") extends CPObjectiveUnit(objVar,n) {

  def domBest: Int = objVar.max
  def domWorst: Int = objVar.min 
  def isMax: Boolean = true
  def isMin: Boolean = false
  def updateToBest(best: Int, delta: Int = 0): CPOutcome = objVar.updateMin(best+delta)   
  def bestBound: Int = ub
  
  // Init best
  best = Int.MinValue
}
