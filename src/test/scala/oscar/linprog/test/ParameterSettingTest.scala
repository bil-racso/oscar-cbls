/**
 * *****************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 * ****************************************************************************
 */

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

    val lp = new LPSolver(LPSolverLib.lp_solve)

    lp.solver.configFile = configLP
    val x = new LPVar(lp, "x", 100, 200)
    val y = new LPVar(lp, "y", 80, 170)

    lp.maximize(-2 * x + 5 * y) subjectTo {
      lp.add(y >= -x + 200)
    }

    x.value should equal(Some(100))
    y.value should equal(Some(170))
    lp.getObjectiveValue() should equal(650)
    lp.status should equal(LPStatus.OPTIMAL)
    lp.checkConstraints() should be(true)

    val lpModel: LpSolve = lp.solver match {
      case s: LPSolve => s.lp
      case _ => null
    }

    lpModel.getPivoting should equal(34)
    lpModel.getPresolve should equal(3)

    lp.release()
    configLP.delete
  }
  test("Config file for Gurobi") {

    val configLP = new java.io.File("GurobiParam.txt")
    val writer = new java.io.PrintWriter(new java.io.FileWriter(configLP))

    writer.print("Presolve 2 \n PreDepRow 0 \n Method 3 \n")
    writer.flush
    writer.close

    val lp = new LPSolver(LPSolverLib.gurobi)

    lp.solver.configFile = configLP
    val x = new LPVar(lp, "x", 100, 200)
    val y = new LPVar(lp, "y", 80, 170)

    lp.maximize(-2 * x + 5 * y) subjectTo {
      lp.add(y >= -x + 200)
    }

    x.value should equal(Some(100))
    y.value should equal(Some(170))
    lp.getObjectiveValue() should equal(650)
    lp.status should equal(LPStatus.OPTIMAL)
    lp.checkConstraints() should be(true)

    val grbModel: GRBModel = lp.solver match {
      case s: GurobiLP => s.model
      case _ => null
    }
    grbModel.getEnv.get(GRB.IntParam.Presolve) should equal(2)
    grbModel.getEnv.get(GRB.IntParam.Method) should equal(3)
    grbModel.getEnv.get(GRB.IntParam.PreDepRow) should equal(0)

    lp.release()
    configLP.delete

  }
}

