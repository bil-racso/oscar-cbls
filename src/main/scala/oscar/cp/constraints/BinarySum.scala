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
import oscar.reversible._
import oscar.cp.core.CPOutcome._

/**
 * x + y = z 
 * @author Pierre Schaus pschaus@gmail.com
 */
class BinarySum(val x: CPVarInt, val y: CPVarInt, val z: CPVarInt) extends Constraint(x.s, "BinarySum") {

  
  override def setup(l: CPPropagStrength): CPOutcome = {
    x.callPropagateWhenBoundsChange(this)
    y.callPropagateWhenBoundsChange(this)
    z.callPropagateWhenBoundsChange(this)
    propagate()
  }

  override def propagate(): CPOutcome = {
    if (z.updateMax(x.max + y.max) == Failure) Failure
    else if (z.updateMin(x.min + y.min) == Failure) Failure
    else if (x.updateMax(z.max - y.min) == Failure) Failure
    else if (x.updateMin(z.min - y.max) == Failure) Failure
    else if (y.updateMax(z.max - x.min) == Failure) Failure
    else if (y.updateMin(z.min - x.max) == Failure) Failure
    else Suspend
  }
}


