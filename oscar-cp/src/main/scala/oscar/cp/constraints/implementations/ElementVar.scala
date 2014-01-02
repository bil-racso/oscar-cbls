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

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.core._
import oscar.cp.core.CPOutcome._


/**
 * Element Constraint: y(x) == z
 * Use CPPropagStrength.Strong to have GAC propagation, otherwise BC propagation is used.
 * @author Pierre Schaus - pschaus@gmail.com
 */
class ElementVar(val y: Array[CPVarInt], val x: CPVarInt, val z: CPVarInt) extends Constraint(y(0).s, "ElementVar") {
    


  override def setup(l: CPPropagStrength): CPOutcome = {
    if (l == CPPropagStrength.Strong) {
      if (s.post(new ElementVarAC(y,x,z)) == Failure) return Failure
      else Success
    } else {
      if (s.post(new ElementVarBC(y,x,z)) == Failure) return Failure
      else Success      
    }

  }
  
 

}
