/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.linprog.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._

/**
 * MIP Testing
 */
class MIPTest extends FunSuite with ShouldMatchers {

  test("mip test 1") {
    for (lib <- solvers) {

      implicit val mip = MIPSolver(lib)
      val x = MIPVar("x", 0, 100)
      val y = MIPVar("y", 0 to 100)

      maximize(8 * x + 12 * y)
      add(10 * x + 20 * y <= 140)
      add(6 * x + 8 * y <= 72)
      start()

      status should equal(LPStatus.OPTIMAL)
      x.value.get should be(8.0 plusOrMinus 0.00001)
      y.value should equal(Some(3))
      release()

    }
  }

  test("mip test 2: update constraints rhs") {
    for (lib <- solvers) {
      println("===================================================================================lib:"+lib)
      implicit val mip = MIPSolver(lib)
      val x = MIPVar("x", 0, 100)
      val y = MIPVar("y", 0 to 100)

      val cons: LPConstraint = mip.add(10 * x + 20 * y <= 140)
      val cons2 = mip.add(8 * x + 6 * y <= 96)

      maximize(8 * x + 12 * y)
      add(6 * x + 8 * y <= 72)
      start()
      
      status should equal(LPStatus.OPTIMAL)
      x.value.get should be(8.0 plusOrMinus 0.00001)
      y.value should equal(Some(3))

      if (lib != LPSolverLib.glpk) { // update not yet implemented in glpk
        mip.updateRhs(cons, 120.0)
        mip.solveModel
        mip.status should equal(LPStatus.OPTIMAL)
        x.value.get should be(12.0 plusOrMinus 0.00001)
        y.value should equal(Some(0))

        mip.updateRhs(cons2, 80.0)
        mip.solveModel
        mip.status should equal(LPStatus.OPTIMAL)
        x.value.get should be(8.0 plusOrMinus 0.00001)
        y.value should equal(Some(2))

        mip.updateRhs(cons, 140.0)
        mip.solveModel
        mip.status should equal(LPStatus.OPTIMAL)
        x.value.get should be(7.75 plusOrMinus 0.00001)
        y.value should equal(Some(3))
      }
      

      release()
    }
  }

  test("mip test 3: update coeficient in constraint") {
    for (lib <- solvers) {

      implicit val mip = MIPSolver(lib)
      val x = MIPVar( "x", 0, 100)
      val y = MIPVar("y", 0 to 100)
      val cons: LPConstraint = add(10 * x + 20 * y <= 140)
      val cons2 = add(8 * x + 6 * y <= 96)

      maximize(8 * x + 12 * y)
      add(6 * x + 8 * y <= 72)
      start()

      mip.status should equal(LPStatus.OPTIMAL)
      x.value.get should be(8.0 plusOrMinus 0.00001)
      y.value should equal(Some(3))

      if (lib != LPSolverLib.glpk) { // update not yet implemented in glpk
        mip.updateCoef(cons, x, 1000.0)
        mip.solveModel
        mip.status should equal(LPStatus.OPTIMAL)
        x.value.get should be(0.0 plusOrMinus 0.00001)
        y.value should equal(Some(7))

        mip.updateCoef(cons, x, 10.0)
        mip.solveModel
        mip.status should equal(LPStatus.OPTIMAL)
        x.value.get should be(8.0 plusOrMinus 0.00001)
        y.value should equal(Some(3))

        mip.updateCoef(cons, y, 10.0)
        mip.solveModel
        mip.status should equal(LPStatus.OPTIMAL)
        x.value.get should be(0.0 plusOrMinus 0.00001)
        y.value should equal(Some(9))
      }

    }
  }
  test("mip test 4: update coeficient and rhs in constraint") {
    for (lib <- solvers) {

      implicit val mip = MIPSolver(lib)
      val x = MIPVar("x", 0, 100)
      val y = MIPVar("y", 0 to 100)
      val cons: LPConstraint = add(10 * x + 20 * y <= 140)
      val cons2 = add(8 * x + 6 * y <= 96)

      maximize(8 * x + 12 * y)
      add(6 * x + 8 * y <= 72)
      start()
      
      status should equal(LPStatus.OPTIMAL)
      x.value.get should be(8.0 plusOrMinus 0.00001)
      y.value should equal(Some(3))

      if (lib != LPSolverLib.glpk) { // update not yet implemented in glpk
        mip.updateCoef(cons, y, 10.0)
        mip.updateRhs(cons2, 30)
        mip.solveModel
        status should equal(LPStatus.OPTIMAL)
        x.value.get should be(0.0 plusOrMinus 0.00001)
        y.value should equal(Some(5))
      }

      mip.release()

    }
  }
}

