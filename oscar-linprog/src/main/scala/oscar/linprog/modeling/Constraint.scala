package oscar.linprog.modeling

import oscar.algebra.LinearExpression
import oscar.linprog.interface.{LPSolverInterface, MIPSolverInterface, MPSolverInterface}

/**
 * Represents a constraint in a mathematical programming model.
 *
 * @author acrucifix acr@n-side.com
 */
abstract class AbstractMPConstraint(solver: MPSolverInterface, val index: Int, val name: String)

sealed abstract class Sense(val symbol: String)
case object LQ extends Sense("<=")
case object EQ extends Sense("==")
case object GQ extends Sense(">=")

class LinearConstraint(solver: MPSolverInterface with LPSolverInterface, index: Int, name: String, val expression: LinearExpression, val sense: Sense) extends AbstractMPConstraint(solver, index, name) {

}

class SOS1Constraint(solver: MPSolverInterface with MIPSolverInterface, index: Int, name: String, val vars: Seq[AbstractMPVar], val weights: Seq[Double]) extends AbstractMPConstraint(solver, index, name) {

}
