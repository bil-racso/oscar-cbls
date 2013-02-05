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

import oscar.search._
import oscar.cp.core._
import oscar.cp.modeling._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPObjectiveUnit(val objVar: CPVarInt, val n: String = "") extends Constraint(objVar.store, "objective"+n) with Objective {

  import TightenType._
  
  protected var lb = Int.MinValue
  protected var ub = Int.MaxValue
  
  protected var tightenType = StrongTighten

  var best = 0 // best value so far (the one recorded on last tighten)
  
  def tightenMode_=(t: TightenType.Value) = {
    tightenType = t
  }
  
  def tightenMode = tightenType
  
  override def tighten() = {
    if (!objVar.isBound) {
      throw new RuntimeException(name+" not bound:" + objVar)
    }
    best = objVar.value
    if (!s.silent && tightenType != NoTighten) println(name+" tightened to " + best + " lb:"+  lb) 
  }


  def relax() = {}

  def restoreBest() = {}

  def isOptimum() = false

  def isOK() = s.propagate(this) != CPOutcome.Failure

  override def toString = "best value:"+best+" tightening:"+tightenType
  
  
  // constraint methods
  
  def filter() = propagate()

  override def setup(l: CPPropagStrength): CPOutcome = {
    lb = objVar.min
    ub = objVar.max
    objVar.callPropagateWhenBoundsChange(this)
    propagate()
  }
  
  

}
