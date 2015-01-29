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

import oscar.cp.core.variables.CPIntVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPPropagStrength._
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

class EqCons(x: CPIntVar, v: Int) extends Constraint(x.store, "Equality") {
  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (x.assign(v) == Failure) Failure
    else Success
  }
}

// FIXME: Equality does not ensure its consistency level at setup
// FIXME: Bounds should be updated with an inner fix point without creating useless L1 events

class Eq(x: CPIntVar, y: CPIntVar) extends Constraint(x.store, "Equality") {

  //idempotent = true
  
  final override def setup(l: CPPropagStrength): CPOutcome = {

    // Assigned variables
    if (y.isBound) {
      if (x.assign(y.min) == Failure) Failure
      else Success
    } else if (x.isBound) {
      if (y.assign(x.min) == Failure) Failure
      else Success
    } 
    
    // Update the bounds
    else if (x.updateMin(y.min) == Failure) Failure
    else if (x.updateMax(y.max) == Failure) Failure
    else if (y.updateMin(x.min) == Failure) Failure
    else if (y.updateMax(x.max) == Failure) Failure

    else {
      // Remove inner values
      if (l == Strong) {

        var i = x.min
        while (i <= x.max) {
          if (x.hasValue(i) && !y.hasValue(i)) {
            if (x.removeValue(i) == Failure) return Failure
          }
          i += 1
        }
        
        i = y.min
        while (i <= y.max) {
          if (y.hasValue(i) && !x.hasValue(i)) {
            if (y.removeValue(i) == Failure) return Failure
          }
          i += 1
        }
      }

      // Register the constraint
      x.callValBindWhenBind(this)
      y.callValBindWhenBind(this)

      if (x.size > 2 || y.size > 2) {
        if (l == Strong) {
          x.callValRemoveWhenValueIsRemoved(this)
          y.callValRemoveWhenValueIsRemoved(this)
        } else {
          x.callUpdateBoundsWhenBoundsChange(this)
          y.callUpdateBoundsWhenBoundsChange(this)
        }
      }

      Suspend
    }
  }

  @inline final override def valBind(intVar: CPIntVar): CPOutcome = {
    if (intVar == x) {
      if (y.assign(x.min) == Failure) Failure
      else Success
    } else if (intVar == y) {
      if (x.assign(y.min) == Failure) Failure
      else Success
    } else sys.error("unknown variable")
  }

  @inline final override def updateBounds(intVar: CPIntVar): CPOutcome = {
    if (intVar == y) {
      if (x.updateMax(y.max) == Failure) Failure
      else if (x.updateMin(y.min) == Failure) Failure
      else Suspend
    } else if (intVar == x) {
      if (y.updateMax(x.max) == Failure) Failure
      else if (y.updateMin(x.min) == Failure) Failure
      else Suspend
    } else sys.error("unknown variable")
  }

  @inline final override def valRemove(intVar: CPIntVar, value: Int): CPOutcome = {
    if (intVar == x) y.removeValue(value)
    else if (intVar == y) x.removeValue(value)
    else sys.error("unknown variable")
  }
}