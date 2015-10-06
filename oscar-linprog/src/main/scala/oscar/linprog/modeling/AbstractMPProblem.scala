package oscar.linprog.modeling

import oscar.linprog.interface.MPSolverInterface

/**
 * Represents a mathematical programming problem.
 *
 * @author acrucifix acr@n-side.com
 */
class AbstractMPProblem(solver: MPSolverInterface) {

  protected val variables = collection.mutable.Map[Int, AbstractMPVar[_]]()
  protected val constraints = collection.mutable.Map[Int, AbstractMPConstraint]()


  /**
   * Returns the MPVar corresponding to the given index
   */
  def variable(index: Int) = variables(index)
}
