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

package oscar.linprog

import oscar.algebra._

/**
 * Helper functions to build models within the context of an implicit solver
 */
package object modeling {

  def minimize(expr: LinearExpression)(implicit solver: MPSolver[_]) = solver.optimize(expr, min = true)
  def maximize(expr: LinearExpression)(implicit solver: MPSolver[_]) = solver.optimize(expr, min = false)

  def add(cstr: LinearConstraintExpression, name: String = "")(implicit solver: MPSolver[_]): LinearConstraint = {
    val n =
      if(name == "") "cstr" + solver.nLinearConstraints
      else name

    new LinearConstraint(n, cstr)
  }
}
