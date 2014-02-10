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

import java.io.{File, FileWriter, PrintWriter}
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import gurobi.{GRB, GRBModel}
import lpsolve.LpSolve
import oscar.algebra.{double2const, int2const}
import oscar.linprog.modeling.{GurobiLP, LPFloatVar, LPSolve, LPSolver, LPSolverLib, LPStatus, MIPFloatVar, MIPIntVar, MIPSolver, add, canInstantiateSolver, checkConstraints, maximize, objectiveValue, release, start, status}
import lpsolve.LpSolveException

/**
 * LPTesting
 */
class ParameterSettingTest extends FunSuite with ShouldMatchers {

  test("Config file for LPSolve") {

    val configLP = new File("LPParam.ini")
    val writer = new PrintWriter(new FileWriter(configLP))
    // The correct header must be specified
    writer.println("[Default]")
    writer.println("pivoting=PRICER_DEVEX + PRICE_ADAPTIVE")
    writer.println("presolve=PRESOLVE_COLS+PRESOLVE_ROWS")
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
  
  test("Incorrect Config file for LPSolve") {

    val configLP = new File("LPParam.ini")
    val writer = new PrintWriter(new FileWriter(configLP))
    // The correct header must be specified
    writer.println("[Default]")
    writer.println("presolve=NOT_A_VALID_VALUE")
    writer.flush
    writer.close

    implicit val mip = new MIPSolver(LPSolverLib.lp_solve)
    mip.solver.configFile = configLP
    
    val x0 = MIPFloatVar(mip, "x0", 0, 40)
    val x1 = MIPIntVar(mip, "x1", 0 to 1000) // can take integer value in range[0 .. 1000]
    val x2 = MIPIntVar(mip, "x2", 0 until 18) // can take integer value in range[0 .. 17] 
    val x3 = MIPFloatVar(mip, "x3", 2, 3)

    maximize(x0 + 2 * x1 + 3 * x2 + x3)
    add(-1 * x0 + x1 + x2 + 10 * x3 <= 20)
    add(x0 - 3.0 * x1 + x2 <= 30)
    add(x1 - 3.5 * x3 == 0)
    try {
    	mip.start()
    } catch {
      case _ : LpSolveException => fail("Incorrect param files should not leak exceptions")
    }
    mip.release()
    configLP.delete
  }  
  
  test("Config file for LPSolve: Bug #72 ") {

    val configLP = new File("LPParam.ini")
    val writer = new PrintWriter(new FileWriter(configLP))
    // The correct header must be specified
    writer.println("[Default]")
    writer.println("break_at_first=1")
    writer.flush
    writer.close

    implicit val mip = new MIPSolver(LPSolverLib.lp_solve)
    mip.solver.configFile = configLP

    val x0 = MIPFloatVar(mip, "x0", 0, 40)
    val x1 = MIPIntVar(mip, "x1", 0 to 1000) // can take integer value in range[0 .. 1000]
    val x2 = MIPIntVar(mip, "x2", 0 until 18) // can take integer value in range[0 .. 17] 
    val x3 = MIPFloatVar(mip, "x3", 2, 3)

    maximize(x0 + 2 * x1 + 3 * x2 + x3)
    add(-1 * x0 + x1 + x2 + 10 * x3 <= 20)
    add(x0 - 3.0 * x1 + x2 <= 30)
    add(x1 - 3.5 * x3 == 0)
    mip.start()
    mip.release()
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

