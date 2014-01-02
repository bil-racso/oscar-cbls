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

import oscar.cp.core.CPVarSet
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPOutcome

class Requires(val X: CPVarSet, v: Int) extends Constraint(X.store, "Set requires") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    X.requires(v)
  }

}


class Excludes(val X: CPVarSet, v: Int) extends Constraint(X.store, "Set excludes") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    X.excludes(v)
  }

}
