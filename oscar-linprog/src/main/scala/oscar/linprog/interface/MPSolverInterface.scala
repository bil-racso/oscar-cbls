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

package oscar.linprog.interface

import java.nio.file.Path

import oscar.linprog.enums.{EndStatus, ModelExportFormat, SolutionQuality}

/**
 * Solver-independent low-level interface describing a solver for mathematical programming problems
 * that should be implemented by the individual solvers to make them available in OscaR.
 *
 * @author acrucifix acr@n-side.com
 */
abstract class MPSolverInterface {
  // NOTE:
  // "varId" refers the column (starting at 0) representing the variable in the matrix of the problem.
  // "cstrId" refers the row (starting at 0) representing the constraint in the matrix of the problem.

  /**
   * The type of the underlying solver
   */
  type Solver

  /**
   * Returns an object that can be used to access the solver-specific API.
   */
  def rawSolver: Solver

  /**
   * Returns the name of the solver implementing this interface.
   */
  def solverName: String

  /**
   * Returns the name of the model.
   */
  def modelName: String

  /**
   * Sets the name of the model in the solver.
   */
  def modelName_=(value: String)

  /**
   * Defines the objective function of the model as ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)]''.
   *
   * @param coefs are the coefficients of the linear term
   * @param varIds indicates to which variable the coefficients refer to
   */
  def addObjective(coefs: Array[Double], varIds: Array[Int]): Unit

  /**
   * Sets the coefficient of the variable in the objective to the given value.
   */
  def setObjCoef(varId: Int, coef: Double): Unit

  /**
   * Sets the problem as a minimization (true) or maximization (false) problem
   */
  def setOptimizationDirection(minimize: Boolean): Unit

  /**
   * Adds a new variable to the model.
   *
   * This function can be used for column generation by specifying
   * the three parameters '''objCoef''', '''cstrCoef''', '''cstrId'''.
   * Note: all three parameters should be defined otherwise an [[IllegalArgumentException]] is thrown.
   *
   * @param name the name of the variable in the model
   * @param lb the lower bound (default is -Inf)
   * @param ub the upper bound (default is +Inf)
   * @param objCoef the coefficient in the objective function (optional)
   * @param cstrCoefs the non-zero constraint coefficients (optional)
   * @param cstrIds the indices of the corresponding linear constraints (optional)
   *
   * @return the index of the variable (column number) in the model
   */
  def addVariable(name: String, lb: Double = Double.NegativeInfinity, ub: Double = Double.PositiveInfinity,
    objCoef: Option[Double] = None, cstrCoefs: Option[Array[Double]] = None, cstrIds: Option[Array[Int]] = None): Int

  /**
   * Returns the lower bound of the variable.
   */
  def getVarLB(varId: Int): Double

  /**
   * Sets the lower bound of the variable to the given value.
   */
  def setVarLB(varId: Int, lb: Double)

  /**
   * Removes the lower bound on the variable (sets it to -Inf).
   */
  def setUnboundedVarLB(varId: Int) = setVarLB(varId, Double.NegativeInfinity)

  /**
   * Returns the upper bound of the variable.
   */
  def getVarUB(varId: Int): Double

  /**
   * Sets the upper bound of the variable to the given value.
   */
  def setVarUB(varId: Int, ub: Double)

  /**
   * Removes the upper bound on the variable (sets it to +Inf).
   */
  def setUnboundedVarUB(varId: Int) = setVarUB(varId, Double.PositiveInfinity)

  /**
   * Returns the bounds of the variable as a pair (lower, upper).
   */
  def getVarBounds(varId: Int): (Double, Double) = (getVarLB(varId), getVarUB(varId))

  /**
   * Sets the bounds of the variable to the given values.
   */
  def setVarBounds(varId: Int, lb: Double, ub: Double) = {
    setVarLB(varId, lb)
    setVarUB(varId, ub)
  }

  /**
   * Adds a new linear constraint (row) to the model.
   *
   * @param name the name of the constraint in the model
   * @param coefs the non-zero coefficients
   * @param varIds the indices of the corresponding variables
   * @param sense the sense of the constraint (one of ''<='', ''=='', ''>='')
   * @param rhs the right hand side
   *
   * @return the index of the constraint (row number) in the model
   */
  def addConstraint(name: String, coefs: Array[Double], varIds: Array[Int], sense: String, rhs: Double): Int

  /**
   * Sets the coefficient of the variable in the corresponding constraint to the specified value.
   */
  def setCstrCoef(cstrId: Int, varId: Int, coef: Double): Unit

  /**
   * Sets the right hand side (constant term) of the specified constraint to the given value.
   */
  def setCstrRhs(cstrId: Int, rhs: Double): Unit

  /**
   * Returns the current number of variables in the model.
   */
  def nVariables: Int

  /**
   * Returns the current number of linear constraints in the model.
   */
  def nLinearConstraints: Int

  /**
   * Commits to the solver the last changes done to the model.
   */
  def updateModel()

  /**
   * Saves the problem to the file at the given path in the given format.
   */
  def exportModel(filepath: Path, format: ModelExportFormat): Unit

  // TODO exportSolution

  /**
   * Returns the status after termination of the optimization.
   */
  def endStatus: EndStatus

  /**
   * Returns true if the solver has found a valid solution
   */
  def hasSolution: Boolean

  /**
   * Returns the [[SolutionQuality]] of the solution found.
   */
  def solutionQuality: SolutionQuality

  /**
   * Returns the objective value of the solution found by the solver.
   *
   * In particular, this may be the objective value for the best feasible solution if optimality is not attained
   * or proven.
   */
  def objectiveValue: Double

  /**
   * Returns the best known bound on the optimal objective value.
   */
  def objectiveBound: Double

  /**
   * Returns the solution found by the solver with one entry for each variable (column).
   *
   * In particular, this may be the best feasible solution found so far if optimality is not attained or proven.
   */
  def solution: Array[Double]

  /**
   * Returns the value of the given variable in the solution.
   */
  def getVarValue(varId: Int): Double

  /**
   * Returns the vector of the values taken by the linear constraints in the solution found by the solver.
   */
  def cstrSolution: Array[Double]

  /**
   * Returns the value taken by the specified linear constraint in the solution.
   */
  def getCstrValue(cstrId: Int): Double

  /**
   * Solves the optimization problem.
   *
   * This operation is blocking until the optimization ends.
   */
  def optimize(): EndStatus

  /**
   * Releases the memory of this solver.
   */
  def release(): Unit

  /**
   * Returns true if the solver has been released.
   */
  def released: Boolean

  /**
   * Configure the solver using the configuration file located at the given ''absolute'' path.
   */
  def configure(absPath: Path)

  /**
   * Adds a time limit to the solver.
   *
   * @param nSeconds the time limit duration in seconds.
   */
  def setTimeout(nSeconds: Long)
}
