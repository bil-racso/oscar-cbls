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
import org.scalatest.matchers.ShouldMatchers
import oscar.linprog.modeling._


import oscar.algebra._

/**
 * LPTesting
 */
class LPTest extends FunSuite with ShouldMatchers {

  test("lp test 1") {
    for (lib <- solvers) {
      implicit val lp = LPSolver(lib)
      val x = new LPFloatVar(lp, "x", 100, 200)
      val y = new LPFloatVar(lp, "y", 80, 170)

      maximize(-2 * x + 5 * y)
      add(y >= -x + 200)
      start()

      x.value should equal(Some(100))
      y.value should equal(Some(170))
      objectiveValue.get should equal(650)
      status should equal(LPStatus.OPTIMAL)
      checkConstraints() should be(true)
      release()
    }
  }

  test("lp test 2") {
    for (lib <- solvers) {
      implicit val lp = LPSolver(lib)
      val x = LPFloatVar(lp, "x", 100, 200)
      val y = LPFloatVar(lp, "y", 80, 170)

      minimize(-2 * x + 5 * y)
      add(y >= -x + 200)
      start()

      x.value should equal(Some(200))
      y.value should equal(Some(80))
      objectiveValue.get should equal(0)
      status should equal(LPStatus.OPTIMAL)
      checkConstraints() should be(true)
      release()
    }
  }

  test("lp test 3") {
    for (lib <- solvers) {
      implicit val lp = LPSolver(lib)
      val x = LPFloatVar("x")
      val y = LPFloatVar("y", 80, 170)

      minimize(-2 * x + 5 * y)
      add(y >= -x + 200)
      start()
      // the solution is infeasible but some solver consider it dual infeasible
      status should (equal(LPStatus.UNBOUNDED) or equal(LPStatus.INFEASIBLE))
      release()
    }
  }

  test("lp test 4") {
    for (lib <- solvers) {
      implicit val lp = LPSolver(lib)
      val x = LPFloatVar( "x", 100, 200)
      val y = LPFloatVar("y", 80, 170)
      minimize(-2 * x + 5 * y)
      val z = new LPFloatVar(lp, "z", 80, 170)
      add(z >= 170)
      add(y >= -x + 200)
      start()

      x.value should equal(Some(200))
      y.value should equal(Some(80))
      z.value should equal(Some(170))
      objectiveValue.get should equal(0)
      status should equal(LPStatus.OPTIMAL)
      release()
    }
  }

  test("lp test 5") {
    for (lib <- solvers) {
      implicit val lp = new LPSolver(lib)
      val x = LPFloatVar("x", 100, 200)
      val y = LPFloatVar("y", 80, 170)
      minimize(-2 * x + 5 * y) 
      add(y >= -x + 200)
      start()

      x.value should equal(Some(200))
      y.value should equal(Some(80))
      objectiveValue.get should equal(0)
      status should equal(LPStatus.OPTIMAL)
      release()
    }
  }

  test("lp test 6") {
    for (lib <- solvers) {
      implicit val lp = new LPSolver(lib)
      val x = LPFloatVar("x", 0, 10)
      val y = LPFloatVar("y", 0, 10)
      maximize(x + y) 
      add(x + y >= 5)
      start()

      x.value should equal(Some(10))
      y.value should equal(Some(10))
      objectiveValue.get should equal(20)
      lp.status should equal(LPStatus.OPTIMAL)

      x.setBounds(0, 11, true) // change bounds and re-optimize 
      y.setBounds(0, 11, true)

      x.value should equal(Some(11))
      y.value should equal(Some(11))
      objectiveValue.get should equal(22)
      lp.status should equal(LPStatus.OPTIMAL)

      lp.release()
    }
  }

  test("lp test 7") {
    for (lib <- solvers) {
      implicit val lp = LPSolver(lib)
      val x = new LPFloatVar(lp, "x", 0, 10)
      val y = new LPFloatVar(lp, "y", 0, 10)

      var cons: Vector[LPConstraint] = Vector()

      maximize(x + y)
      cons = cons :+ add(x + y >= 5)
      cons = cons :+ add(x + 2 * y <= 25)
      cons = cons :+ add(x + 2 * y <= 30)
      cons = cons :+ add(x + y >= 17.5)
      cons = cons :+ add(x == 10.0)
      
      start()
      x.value.get should be(10.0 plusOrMinus 1e-6)
      y.value.get should be(7.5 plusOrMinus 1e-6)

      cons(0).isTight() should be(false)
      cons(1).isTight() should be(true)
      cons(2).isTight() should be(false)
      cons(3).isTight() should be(true)
      cons(4).isTight() should be(true)

      cons(0).slack should be(12.5 plusOrMinus 1e-6)
      cons(1).slack should be(0.0 plusOrMinus 1e-6)
      cons(2).slack should be(5.0 plusOrMinus 1e-6)
      cons(3).slack should be(0.0 plusOrMinus 1e-6)
      cons(4).slack should be(0.0 plusOrMinus 1e-6)

      cons.foreach(c => c.check() should be(true))

      lp.objectiveValue.get should be(17.5 plusOrMinus 1e-6)
      lp.status should equal(LPStatus.OPTIMAL)

      lp.release()
    }
  }

}

