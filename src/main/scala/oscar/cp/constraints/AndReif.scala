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
 * and_i x_i <--> bi 
 * @author Pierre Schaus pschaus@gmail.com
 */
class AndReif(val X: Array[CPVarBool], val b: CPVarBool) extends Constraint(b.s, "AndReif") {

  override def setup(l: CPPropagStrength): CPOutcome = {
    X.foreach(_.callPropagateWhenBind(this))
    b.callPropagateWhenBind(this)
    propagate()
  }
  
  override def propagate(): CPOutcome = {
    if (b.isBoundTo(1)) {
      for (x <- X) {
        if (x.assign(1) == Failure) return Failure
      }
      Success
    }
    else if (b.isBoundTo(0)) {
      if (X.exists(_.isBoundTo(0))) Success
      else Suspend
    }
    else if (X.exists(_.isBoundTo(0))) {
      b.assign(0)
      Success
    } else if (X.forall(_.isBoundTo(1))) {
      b.assign(1)
      Success
    } else {
      Suspend
    }

  }
}


