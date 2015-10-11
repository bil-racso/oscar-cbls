package oscar.linprog.modeling

import oscar.linprog.interface.MPSolverInterface

/**
 * Brings an implicit [[MPSolver]] in the context for modeling mathematical programming problems.
 */
class MPModel[I <: MPSolverInterface](_solver: MPSolver[I]) {
  implicit val solver: MPSolver[I] = _solver
}