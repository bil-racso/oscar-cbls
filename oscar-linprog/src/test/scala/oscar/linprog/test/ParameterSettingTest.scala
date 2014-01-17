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

import gurobi.{ GRBModel, GRB }
import lpsolve.LpSolve

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.linprog.modeling._

import oscar.linprog._
import oscar.algebra._

/**
 * LPTesting
 */
class ParameterSettingTest extends FunSuite with ShouldMatchers {

  test("Config file for LPSolve") {

    val configLP = new java.io.File("LPParam.ini")
    val writer = new java.io.PrintWriter(new java.io.FileWriter(configLP))
    // The correct header must be specified
    writer.print("[Default]\n")
    writer.print("pivoting=PRICER_DEVEX + PRICE_ADAPTIVE\n" +
      "presolve=PRESOLVE_COLS+PRESOLVE_ROWS ")
    writer.flush
    writer.close

    implicit val lp = new LPSolver(LPSolverLib.lp_solve)

    lp.solver.configFile = configLP
    val x = LPFloatVar("x", 100, 200)
    val y = LPFloatVar("y", 80, 170)

    maximize(-2 * x + 5 * y)
    add(y >= -x + 200)
    start()

    x.value should equal(Some(100))
    y.value should equal(Some(170))
    objectiveValue.get should equal(650)
    status should equal(LPStatus.OPTIMAL)
    checkConstraints() should be(true)

    val lpModel: LpSolve = lp.solver match {
      case s: LPSolve => s.lp
      case _ => null
    }

    lpModel.getPivoting should equal(34)
    lpModel.getPresolve should equal(3)

    release()
    configLP.delete
  }
  test("Config file for Gurobi") {
	assume(canInstantiateSolver(LPSolverLib.gurobi), "The test could not access Gurobi. Check you have it installed.")
    val configLP = new java.io.File("GurobiParam.txt")
    val writer = new java.io.PrintWriter(new java.io.FileWriter(configLP))

    writer.print("Presolve 2 \n PreDepRow 0 \n Method 3 \n")
    writer.flush
    writer.close

    implicit val lp = LPSolver(LPSolverLib.gurobi)

    lp.solver.configFile = configLP
    val x = LPFloatVar("x", 100, 200)
    val y = LPFloatVar("y", 80, 170)

    maximize(-2 * x + 5 * y) 
    add(y >= -x + 200)
    start()

    x.value should equal(Some(100))
    y.value should equal(Some(170))
    objectiveValue.get should equal(650)
    status should equal(LPStatus.OPTIMAL)
    checkConstraints() should be(true)

    val grbModel: GRBModel = lp.solver match {
      case s: GurobiLP => s.model
      case _ => null
    }
    grbModel.getEnv.get(GRB.IntParam.Presolve) should equal(2)
    grbModel.getEnv.get(GRB.IntParam.Method) should equal(3)
    grbModel.getEnv.get(GRB.IntParam.PreDepRow) should equal(0)

    release()
    configLP.delete

  }
}

