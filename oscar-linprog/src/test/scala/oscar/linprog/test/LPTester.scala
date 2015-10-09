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


package oscar.linprog.test

import org.scalatest.FunSuite
import oscar.linprog.interface.{SolutionFound, LP}
import oscar.linprog.modeling._
import oscar.algebra._
import org.scalatest.Matchers


class LPTester extends FunSuite with Matchers {
  test("Feasible model 1") {
    for (_solver <- MPSolver.lpSolvers) {
      implicit val solver: MPSolver[_] = _solver

      val x = FloatVar("x", 100, 200)
      val y = FloatVar("y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(y >= -x + 200)

      val endStatus = solver.solve

      endStatus should equal(SolutionFound)

      x.value should equal(Some(100))
      y.value should equal(Some(170))

      solver.release()
    }
  }
}
