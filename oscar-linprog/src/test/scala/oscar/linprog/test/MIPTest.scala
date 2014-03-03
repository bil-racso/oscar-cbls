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

import java.io.File

/**
 * MIP Testing
 */
class MIPTest extends FunSuite with ShouldMatchers {

  test("mip test 1") {
    for (lib <- solvers) {
      implicit val mip = MIPSolver(lib)
      mip.name = "Mip Test 1"
      val x = MIPFloatVar("x", 0, 100)
      val y = MIPIntVar("y", 0 to 100)

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

  test("MIP test 2: Update constraints rhs") {
    for (lib <- solvers) {
      implicit val mip = MIPSolver(lib)
      mip.name = "MIP Test 2"
      val x = MIPFloatVar("x", 0, 100)
      val y = MIPIntVar("y", 0 to 100)

      val cons: LPConstraint = mip.add(10 * x + 20 * y <= 140)
      val cons2 = mip.add(8 * x + 6 * y <= 96)

      maximize(8 * x + 12 * y)
      add(6 * x + 8 * y <= 72)
      start()
      
      status should equal(LPStatus.OPTIMAL)
      x.value.get should be(8.0 plusOrMinus 0.00001)
      y.value should equal(Some(3))

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
      
      release()
    }
  }

  test("MIP Test 3: update coeficient in constraint") {
    for (lib <- solvers) {

      implicit val mip = MIPSolver(lib)
      mip.name = "MIP Test 3"
      val x = MIPFloatVar( "x", 0, 100)
      val y = MIPIntVar("y", 0 to 100)
      val cons: LPConstraint = add(10 * x + 20 * y <= 140)
      val cons2 = add(8 * x + 6 * y <= 96)

      maximize(8 * x + 12 * y)
      add(6 * x + 8 * y <= 72)
      start()

      mip.status should equal(LPStatus.OPTIMAL)
      x.value.get should be(8.0 plusOrMinus 0.00001)
      y.value should equal(Some(3))

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
      mip.release()
    }
  }
  
  test("MIP Test 4: update coefficient and rhs in constraint") {
    for (lib <- solvers) {

      implicit val mip = MIPSolver(lib)
      mip.name = "MIP Test 4"
      val x = MIPFloatVar("x", 0, 100)
      val y = MIPIntVar("y", 0 to 100)
      val cons: LPConstraint = add(10 * x + 20 * y <= 140)
      val cons2 = add(8 * x + 6 * y <= 96)

      maximize(8 * x + 12 * y)
      add(6 * x + 8 * y <= 72)
      start()
      
      status should equal(LPStatus.OPTIMAL)
      x.value.get should be(8.0 plusOrMinus 0.00001)
      y.value should equal(Some(3))

      mip.updateCoef(cons, y, 10.0)
      mip.updateRhs(cons2, 30)
      mip.solveModel
      status should equal(LPStatus.OPTIMAL)
      x.value.get should be(0.0 plusOrMinus 0.00001)
      y.value should equal(Some(5))

      mip.release()
    }
  }
  
  test("MIP Test 5: test whether 0-1 variables set as binary") {
    for (lib <- solvers) {
        implicit val mip = MIPSolver(lib)
        val x = Array.tabulate(6)(j => MIPIntVar(s"x${j}", 0 to 1))
        val z = 3 * x(0) + 5 * x(1) + 6 * x(2) + 9 * x(3) + 10 * x(4) + 10 * x(5)
        minimize(z)
        add(-2 * x(0) + 6 * x(1) - 3 * x(2) + 4 * x(3) + x(4) - 2 * x(5) >= 2)
        add(-5 * x(0) - 3 * x(1) + x(2) + 3 * x(3) - 2 * x(4) + x(5) >= -2)
        add(5 * x(0) - x(1) + 4 * x(2) -2 * x(3) + 2 * x(4) - x(5) >= 3)    
	x.foreach(_.isBinary() should be(true))
	    
        start()
	status should equal(LPStatus.OPTIMAL)
	z.value should be(Some(11.0))
        release()
    }
  }
}

