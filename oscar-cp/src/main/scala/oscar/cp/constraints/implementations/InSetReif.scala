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
package oscar.cp.constraints.implementations

import oscar.cp.core._
import oscar.algo.reversible._
import oscar.cp.core.CPOutcome._

/**
 * x must be a value of the set
 * @author Pierre Schaus pschaus@gmail.com
 */
class InSetReif(val x: CPVarInt, val set: Set[Int], val b: CPVarBool) extends Constraint(x.s, "InSetReif") {
  val setSize = set.size
  val setMin = set.min
  val setMax = set.max
  val setRange = (setSize == (setMax-setMin+1))
  
  val supportValueInSet = new ReversibleInt(s,0)
  val supportValueNotInSet = new ReversibleInt(s,0)
  
  
  override def setup(l: CPPropagStrength): CPOutcome = {
    
    if (!b.isBound) b.callValBindWhenBind(this)
    else return valBind(b)
    
    if (!x.isBound) x.callValBindWhenBind(this)
    else return valBind(x)
    updateSupportNotInSet()
    updateSupportNotInSet()
    x.callPropagateWhenDomainChanges(this)
    
    propagate()
  }
  
  def updateSupportInSet(): Boolean = {
    if (x.hasValue(supportValueInSet.value)) {
      true
    } else {
      for (v <- set) {
        if (x.hasValue(v)) {
          supportValueInSet.value = v
          return true
        }
      }
      false
    }
  }
  
  def updateSupportNotInSet(): Boolean = {
    if (x.hasValue(supportValueNotInSet.value) && !set.contains(supportValueNotInSet.value)) {
      true
    } else {
      for (v <- x) {
        if (!set.contains(v)) {
          supportValueNotInSet.value = v
          return true
        }
      }
      false
    }
  }  
  
  override def valBind(variable: CPVarInt): CPOutcome = {
    if (b.isTrue) {
      for (v <- x.toSet if !set.contains(v)) {
        if (x.removeValue(v) == Failure) {
          return Failure
        }
      }
    } else if (b.isFalse) {
      for (v <- x.toSet if set.contains(v)) {
        if (x.removeValue(v) == Failure) {
          return Failure
        }
      }
    } else if (x.isBound) {
      val value = if (set.contains(x.value)) 1 else 0
      if (b.assign(value) == Failure) {
        return Failure
      }
    }
    Success
  }

  override def propagate(): CPOutcome = {
    if (x.min > setMax) {
      if (b.assign(0) == Failure) {
        return Failure
      }
      return Success
    } else if (x.max < setMin) {
       if (b.assign(0) == Failure) {
        return Failure
      } 
      return Success
    } else if (setRange && x.min >= setMin && x.max <= setMax) {
       if (b.assign(1) == Failure) {
        return Failure
      }      
    } else {
      val atLeastOneNotInSet = updateSupportNotInSet()
      if (!atLeastOneNotInSet) {
        if (b.assign(1) == Failure) {
          return Failure
        }        
      }
      val atLeastOneInSet = updateSupportInSet()
      if (atLeastOneNotInSet && !atLeastOneInSet) {
        if (b.assign(0) == Failure) {
          return Failure
        }
        return Success
      }
    }
    Suspend
  }

}


