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

import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPIntervalVar
import oscar.cp.core.CPIntervalVar
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
final class DiffVarInterval(x: CPIntervalVar, y: CPIntervalVar) extends Constraint(x.store, "DiffVar") {
  

  final override def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate() == Failure) Failure
    else {
      x.callPropagateWhenBoundsChange(this, false)
      y.callPropagateWhenBoundsChange(this, false)
      Suspend
    }
  }

  @inline final override def propagate(): CPOutcome = {
    if (x.isBound) {
      if (y.min == x.value) {
        if (y.updateMin(x.value+1) == Failure) Failure
        else Success
      }
      else if (y.max == x.value) {
        if (y.updateMax(x.value-1) == Failure) Failure
        else Success
      }
      else CPOutcome.Suspend
    } else if (y.isBound) {
      if (x.min == y.value) {
        if (x.updateMin(y.value+1) == Failure) Failure
        else Success
      }
      else if (x.max == y.value) {
        if (x.updateMax(y.value-1) == Failure) Failure
        else Success
      }      
      else Suspend
    } else Suspend
  }
}