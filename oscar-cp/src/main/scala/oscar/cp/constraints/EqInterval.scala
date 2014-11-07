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

import oscar.cp.core.CPIntervalVar
import oscar.cp.core.CPIntervalVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPPropagStrength._
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

class EqConsInterval(x: CPIntervalVar, v: Int) extends Constraint(x.store, "Equality") {
  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (x.assign(v) == Failure) Failure
    else Success
  }
}

// FIXME: Equality does not ensure its consistency level at setup
// FIXME: Bounds should be updated with an inner fix point without creating useless L1 events

class EqInterval(x: CPIntervalVar, y: CPIntervalVar) extends Constraint(x.store, "Equality") {

  //idempotent = true

  final override def setup(l: CPPropagStrength): CPOutcome = {

    // Assigned variables
    if (y.isBound) {
      if (x.assign(y.value) == Failure) Failure
      else Success
    } else if (x.isBound) {
      if (y.assign(x.value) == Failure) Failure
      else Success
    } // Update the bounds
    else if (x.updateMin(y.min) == Failure) Failure
    else if (x.updateMax(y.max) == Failure) Failure
    else if (y.updateMin(x.min) == Failure) Failure
    else if (y.updateMax(x.max) == Failure) Failure

    x.callUpdateBoundsWhenBoundsChange(this)
    y.callUpdateBoundsWhenBoundsChange(this)

    // Register the constraint
    x.callValBindWhenBind(this)
    y.callValBindWhenBind(this)

    Suspend

  }

  @inline final override def valBind(intVar: CPIntervalVar): CPOutcome = {
    if (intVar == x) {
      if (y.assign(x.value) == Failure) Failure
      else Success
    } else if (intVar == y) {
      if (x.assign(y.value) == Failure) Failure
      else Success
    } else sys.error("unknown variable")
  }

  @inline final override def updateBounds(intVar: CPIntervalVar): CPOutcome = {
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

}