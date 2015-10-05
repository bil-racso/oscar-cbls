package oscar.linprog.interface

/**
 * This interface complements the [[MPSolverInterface]] by adding functions specific
 * to LPs (such as information about the dual).
 *
 * @author acrucifix acr@n-side.com
 */
trait LPSolverInterface { self: MPSolverInterface =>
  /**
   * Returns the reduced costs of the variables in the solution.
   *
   * Note: it is the vector corresponding to the values in the solution of the duals of the variable bounds.
   */
  def reducedCosts: Vector[Double]

  /**
   * Get the reduced cost corresponding to the given variable in the solution.
   *
   * Note: the reduced cost is the value of the dual of the variable bounds.
   */
  def getReducedCost(varId: Int): Double

  /**
   * Returns the vector of the values in the solution of the dual variables corresponding to the linear constraint.
   */
  def cstrDuals: Double

  /**
   * Return the value in the solution of the dual to the given constraint.
   */
  def getCstrDual(cstrId: Int): Double
}
