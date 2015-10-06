package oscar.linprog.interface

/**
 * This interface complements the [[MPSolverInterface]] by adding functions specific
 * to MIPs (such as the addition of SOS constraints).
 *
 * @author acrucifix acr@n-side.com
 */
trait MIPSolverInterface { self: MPSolverInterface =>
  /**
   * Returns true if the specified variable is an integer variable.
   */
  def isInteger(varId: Int): Boolean

  /**
   * Sets the specified variable as an integer variable.
   */
  def setInteger(varId: Int)

  /**
   * Returns true if the specified variable is a binary 0-1 integer variable.
   */
  def isBinary(varId: Int): Boolean

  /**
   * Set the specified variable as a binary 0-1 integer variable.
   */
  def setBinary(varId: Int)

  /**
   * Returns true if the specified variable is a float variable.
   */
  def isFloat(varId: Int): Boolean

  /**
   * Set the specified variable as a float variable.
   */
  def setFloat(varId: Int)

  /**
   * Adds a Specially Ordered Set (SOS) Type 1 constraint. Constrains at most one variable
   * in a collection to be equal to 1. Useful for modelling discrete choices.
   *
   * @param coef the weightings on the variables (these influence the search branching decision)
   * @param varId indicates which variables are included in the SOS constraints
   *
   * @return constraint index
   */
  def addConstraintSOS1(coef: Vector[Double], varId: Vector[Int], name: String): Int
}
