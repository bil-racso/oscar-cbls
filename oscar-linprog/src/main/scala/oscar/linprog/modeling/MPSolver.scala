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

package oscar.linprog.modeling

import java.nio.file.Path

import oscar.algebra.{LinearConstraintExpression, Expression, Const, LinearExpression}
import oscar.linprog.interface._
import oscar.linprog.interface.lpsolve.LPSolve

import scala.util.{Success, Failure, Try}

/**
 * A solver that can be used to solve mathematical programming problems.
 *
 * @author acrucifix acr@n-side.com
 */
class MPSolver[I <: MPSolverInterface](val solverInterface: I) {

  protected def dirty: Boolean = solveStatus == NotSolved
  protected def setDirty() = _solveStatus = NotSolved

  protected def asSuccessIfSolFound[B](value: B): Try[B] = endStatus.flatMap { status =>
    if (status == SolutionFound) Success(value)
    else                         Failure(NoSolutionFound(status))
  }

  /**
   * Returns the name of the current model
   */
  def modelName = solverInterface.modelName

  /**
   * Sets the name of the current model to the given value
   */
  def modelName_=(value: String) = solverInterface.modelName = value


  /* OBJECTIVE */

  private var _objective: LinearExpression = Const(1)

  /**
   * Returns the [[LinearExpression]] representing the current objective of the problem
   */
  def objective: LinearExpression = _objective

  /**
   * Replaces the objective to be optimized by the given [[LinearExpression]]
   */
  def objective_=(value: LinearExpression) = {
    setDirty()

    _objective = value

    val coefs = value.coef.values.toArray
    val varIds = value.coef.keys.map(v => variableColumn(v.name)).toArray
    solverInterface.addObjective(coefs, varIds)
  }

  private var _minimize: Boolean = true

  /**
   * Returns true if the problem is a minimization problem. That is the target is to minimize the given objective.
   */
  def minimize: Boolean = _minimize

  /**
   * Sets the problem as a minimization (true) or maximization (false) problem.
   */
  def minimize_=(value: Boolean) = {
    setDirty()

    _minimize = value
    solverInterface.setOptimizationDirection(value)
  }

  /**
   * Sets the optimization objective and direction to the given values.
   *
   * @param obj the new objective expression
   * @param min the new optimization direction (true for minimization and false for maximization)
   */
  def optimize(obj: LinearExpression, min: Boolean) = {
    setDirty()

    objective = obj
    minimize = min
  }

  /**
   * Returns the value of the objective in the solution found to the current model (if any)
   */
  def objectiveValue = asSuccessIfSolFound(solverInterface.objectiveValue)


  /* VARIABLES */

  protected var variables = Map[String, AbstractMPVar[_]]()
  protected var variableColumn = Map[String, Int]()

  def nVariables = variables.size

  protected def register(variable: AbstractMPVar[_], colId: Int): Unit = {
    setDirty()

    variables += (variable.name -> variable)
    variableColumn += (variable.name -> colId)
  }

  /**
   * Adds the given [[FloatVar]] to the problem
   */
  def add(variable: FloatVar) = {
    val colId = solverInterface.addVariable(variable.name, variable.lowerBound, variable.upperBound)

    register(variable, colId)
  }

  /**
   * Adds the given [[IntVar]] to the problem
   */
  def add(variable: IntVar)(implicit ev: I => MIPSolverInterface) = {
    val colId = solverInterface.addIntegerVariable(variable.lowerBound, variable.upperBound, variable.name)

    register(variable, colId)
  }

  /**
   * Adds the given [[BinaryVar]] to the problem
   */
  def add(variable: BinaryVar)(implicit ev: I => MIPSolverInterface) = {
    val colId = solverInterface.addBinaryVariable(variable.name)

    register(variable, colId)
  }

  /**
   * Returns the [[AbstractMPVar]] corresponding to the given name
   */
  def variable(name: String) = variables(name)

  /**
   * Updates the lower bound of the variable with the given name to the given value
   *
   * @param varName the name of the variable that should be updated
   * @param lb the new value of the lower bound for this variable
   */
  def updateLowerBound(varName: String, lb: Double) = {
    setDirty()

    solverInterface.setVarLB(variableColumn(varName), lb)
  }

  /**
   * Updates the upper bound of the variable with the given name to the given value
   *
   * @param varName the name of the variable that should be updated
   * @param ub the new value of the upper bound for this variable
   */
  def updateUpperBound(varName: String, ub: Double) = {
    setDirty()

    solverInterface.setVarUB(variableColumn(varName), ub)
  }

  /**
   * Returns the value of the variable with the given name in the current solution (if any)
   */
  def value(varName: String): Try[Double] = endStatus.flatMap { status =>
    if (status == SolutionFound) Success(solverInterface.getVarValue(variableColumn(varName)))
    else                         Failure(NoSolutionFound(status))
  }


  /* CONSTRAINTS */

  protected var linearConstraints = Map[String, LinearConstraint]()
  protected var linearConstraintRows = Map[String, Int]()

  def nLinearConstraints = linearConstraints.size

  protected def register(linearConstraint: LinearConstraint, rowId: Int): Unit = {
    setDirty()

    linearConstraints += (linearConstraint.name -> linearConstraint)
    linearConstraintRows += (linearConstraint.name -> rowId)
  }

  /**
   * Adds the given [[LinearConstraint]] to the model
   */
  def add(linearConstraint: LinearConstraint) = {
    val coefs = linearConstraint.constraintExpr.linExpr.coef.values.toArray
    val varIds = linearConstraint.constraintExpr.linExpr.coef.keys.map(v => variableColumn(v.name)).toArray

    val rowId = solverInterface.addConstraint(linearConstraint.name, coefs, varIds, linearConstraint.constraintExpr.sense.symbol, -linearConstraint.constraintExpr.linExpr.cte)

    register(linearConstraint, rowId)
  }


  /* SOLVE */

  private var _solveStatus: SolveStatus = NotSolved

  /**
   * Returns the current status of the solve. (see [[SolveStatus]])
   */
  def solveStatus = _solveStatus

  /**
   * Solves the current optimization problem
   *
   * @return the [[EndStatus]] of the solve
   */
  def solve: EndStatus = {
    _solveStatus = Solving
    solverInterface.updateModel()
    val es = solverInterface.optimize()
    _solveStatus = Solved
    _endStatus = Success(es)
    es
  }

  /**
   * Returns true if the current problem has been solved
   */
  def solved: Boolean = solveStatus == Solved

  private var _endStatus: Try[EndStatus] = Failure(NotSolvedYet)

  /**
   * Returns the end status of the last solve (if any)
   */
  def endStatus = _endStatus

  /**
   * Returns true if there is a solution to the current problem
   */
  def hasSolution: Boolean = solved && endStatus == Success(SolutionFound)

  /**
   * Releases the raw solver interfaced by OscaR. This may be needed by some solvers that use native resources.
   */
  def release() = solverInterface.release()

  /**
   * Saves the problem to the file at the given path in the given format.
   */
  def exportModel(filepath: Path, format: ExportFormat): Unit = {
    if(dirty) solverInterface.updateModel()

    solverInterface.exportModel(filepath, format)
  }
}

object MPSolver {
  def lp_solve: MPSolver[LPSolve] = new MPSolver(new LPSolve)
  def lp_solveOption: Option[MPSolver[LPSolve]] =
    try {
      Some(lp_solve)
    } catch {
      case e: UnsatisfiedLinkError => println(e.getMessage); None
      case e: NoClassDefFoundError => println(e.getMessage); None
    }

  lazy val lpSolvers = List(MPSolver.lp_solveOption).flatten
}
