package oscar.linprog.interface

import java.nio.file.Path

/**
 * Solver-independent low-level interface describing a solver for mathematical programming problems
 * that should be implemented by the individual solvers to make them available in OscaR.
 *
 * @author acrucifix acr@n-side.com
 */
abstract class MPSolverInterface(solverOptions: (String, Any)*) {
  /**
   * The type of the underlying solver
   */
  type Solver

  /**
   * Returns an object that can be used to access the solver-specific API.
   */
  def rawSolver: Solver

  /**
   * Returns the name of the model.
   */
  def name: String

  /**
   * Sets the name of the model in the solver.
   */
  def name_=(value: String)

  /**
   * Defines the objective function of the model as ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)]''.
   *
   * @param coef are the coefficients of the linear term
   * @param varId indicates to which variable the coefficients refer to
   * @param minimize set to true if this objective should be minimized (default), false to maximize it
   */
  def addObjective(coef: Array[Double], varId: Array[Int], minimize: Boolean = true): Unit

  /**
   * Sets the coefficient of the variable in the objective to the given value.
   */
  def setObjCoef(varId: Int, coef: Double): Unit

  /**
   * Adds a new variable (column) to the model.
   *
   * @param lb the lower bound (default is -Inf)
   * @param ub the upper bound (default is +Inf)
   * @param name the name of the variable in the model
   * @param objCoef the coefficient in the objective function
   * @param cstrCoef the non-zero constraint coefficients
   * @param cstrId the indices of the corresponding linear constraints
   *
   * @return the index of the variable (column number) in the model
   */
  def addVariable(lb: Double = Double.NegativeInfinity, ub: Double = Double.PositiveInfinity, name: String,
    objCoef: Option[Double] = None, cstrCoef: Option[Vector[Double]] = None, cstrId: Option[Vector[Int]] = None): Int

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
   * @param coef the non-zero coefficients
   * @param varId the indices of the corresponding variables
   * @param rhs the right hand side
   * @param sense the sense of the constraint (one of ''<='', ''=='', ''>='')
   * @param name the name of the constraint in the model
   *
   * @return the index of the constraint (row number) in the model
   */
  def addConstraint(coef: Vector[Double], varId: Vector[Int], rhs: Double, sense: String, name: String): Int

  /**
   * Adds the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] >= rhs'' to the model.
   *
   * @param coef are the coefficients of the linear term
   * @param varId indicates to which variable the coefficients refer to
   *
   * @return the index of the constraint (row number) in the model
   */
  def addConstraintGreaterEqual(coef: Vector[Double], varId: Vector[Int], rhs: Double, name: String): Int =
    addConstraint(coef, varId, rhs, ">=", name)

  /**
   * Adds the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] <= rhs'' to the model.
   *
   * @param coef are the coefficients of the linear term
   * @param varId indicates to which variable the coefficients refer to
   *
   * @return the index of the constraint (row number) in the model
   */
  def addConstraintLessEqual(coef: Vector[Double], varId: Vector[Int], rhs: Double, name: String): Int =
    addConstraint(coef, varId, rhs, "<=", name)

  /**
   * Adds the constraint ''coef(0)*x[col(0)] + ... + coef(n)*x[col(n)] == rhs'' to the model.
   *
   * @param coef are the coefficients of the linear term
   * @param varId indicates to which variable the coefficients refer to
   *
   * @return the index of the constraint (row number) in the model
   */
  def addConstraintEqual(coef: Vector[Double], varId: Vector[Int], rhs: Double, name: String): Int =
    addConstraint(coef, varId, rhs, "==", name)

  /**
   * Sets the coefficient of the variable in the corresponding constraint to the specified value.
   */
  def setCstrCoef(consId: Int, varId: Int, coef: Double): Unit

  /**
   * Sets the right hand side (constant term) of the specified constraint to the given value.
   */
  def setCstrRhs(consId: Int, rhs: Double): Unit

  /**
   * Returns the current number of variables in the model.
   */
  def nVariables: Int

  /**
   * Returns the current number of constraints in the model.
   */
  def nConstraints: Int

  /**
   * Commits to the solver the last changes done to the model.
   */
  def updateModel()

  /**
   * Saves the problem to the file at the given path in the given format.
   */
  def exportModel(filepath: Path, format: ExportFormat): Unit

  /**
   * Returns the status after termination of the optimization.
   */
  def endStatus: EndStatus

  /**
   * Returns true if the solver has found a valid solution
   */
  def hasSolution: Boolean

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
  def solution: Vector[Double]

  /**
   * Returns the value of the given variable in the solution.
   */
  def getVarValue(varId: Int): Double

  /**
   * Returns the vector of the values taken by the linear constraints in the solution found by the solver.
   */
  def cstrSolution: Vector[Double]

  /**
   * Returns the value taken by the specified linear constraint in the solution.
   */
  def getCstrValue(cstrId: Int): Double

  /**
   * Solves the optimization problem.
   */
  def optimize(): EndStatus

  /**
   * Aborts the current optimization (if any).
   */
  def abort(): Unit

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
   */
  def setTimeout(t: Long)

  def setIntParameter(name: String, value: Int)

  def setFloatParameter(name: String, value: Double): Unit

  def setStringParameter(name: String, value: String): Unit
}
