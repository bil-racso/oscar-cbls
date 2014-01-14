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

package object modeling {
  
  val rand = new scala.util.Random(12)
    
  object LPSolverLib extends Enumeration {
    val lp_solve = Value("lp_solve")
    val glpk = Value("glpk")
    val gurobi = Value("gurobi")
  }
  
  // solvers used for test
  lazy val solvers = List(LPSolverLib.lp_solve, LPSolverLib.glpk, LPSolverLib.gurobi).filter(canInstantiateSolver(_))

  def canInstantiateSolver(s: LPSolverLib.Value): Boolean = {
	  try {
	    val lp = LPSolver(s)
	  } catch {
	    case e: UnsatisfiedLinkError => { println(e.getMessage()); return false }
	    case e: NoClassDefFoundError => { println(e.getMessage()); return false }
	  }
	  true
  }
  
  // helper functions to model with an implicit LP/MIPSolver
  def add(constr: LinearConstraint, name: String = "")(implicit linearSolver: AbstractLPSolver) = linearSolver.add(constr,name)
  def addAll(constraints: LinearConstraint*)(implicit linearSolver: AbstractLPSolver) {
    constraints.foreach(add(_))
  }
  
  def start()(implicit linearSolver: AbstractLPSolver) = linearSolver.start()
  def minimize(expr: LinearExpression)(implicit linearSolver: AbstractLPSolver) = linearSolver.minimize(expr)
  def maximize(expr: LinearExpression)(implicit linearSolver: AbstractLPSolver) = linearSolver.maximize(expr)
  def release()(implicit linearSolver: AbstractLPSolver) = linearSolver.release()
  def objectiveValue(implicit linearSolver: AbstractLPSolver) = linearSolver.objectiveValue()
  def status(implicit linearSolver: AbstractLPSolver) = linearSolver.status
  def checkConstraints(tol: Double = 10e-6)(implicit linearSolver: AbstractLPSolver) = linearSolver.checkConstraints(tol)

  
}
