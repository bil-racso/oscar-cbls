package oscar.lcg.modeling

import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.heuristic.Heuristic
import oscar.lcg.core.Literal
import oscar.lcg.core.LiftedBoolean
import oscar.lcg.core.LCGSolver

/** @author Renaud Hartert ren.hartert@gmail.com */
trait SolverUtils {

  final def add(constraint: LCGConstraint)(implicit lcgSolver: LCGSolver): Boolean = lcgSolver.add(constraint)
  
  final def add(literal: Literal)(implicit lcgSolver: LCGSolver): Boolean = lcgSolver.add(literal)
  
  final def onSolution(action: => Unit)(implicit lcgSolver: LCGSolver): Unit = lcgSolver.onSolution(action)

  final def solve(heuristic: Heuristic, stopCondition: => Boolean)(implicit lcgSolver: LCGSolver): LiftedBoolean = lcgSolver.solve(heuristic, stopCondition)
  
  final def solve(heuristic: Heuristic)(implicit lcgSolver: LCGSolver): LiftedBoolean = lcgSolver.solve(heuristic, false)
}