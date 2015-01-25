package oscar.cp.modeling

import oscar.cp.core.CPSolver
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPPropagStrength._
import oscar.cp.core.variables.CPIntervalVar
import oscar.cp.core.variables.CPBoolVar
import oscar.algo.search.SearchNode
import oscar.algo.search.SearchStatistics
import oscar.algo.search.Branching
import oscar.algo.search.Alternative

trait CPSolverUtils {
  
    // helper functions to model with an implicit CPSolver
  def add(constraints: Iterable[_ <: Constraint])(implicit cp: CPSolver): Unit = cp.add(constraints)

  def add(c: Constraint, propagStrengh: CPPropagStrength)(implicit cp: CPSolver): Unit = cp.add(c, propagStrengh)
  def add(c: Constraint)(implicit cp: CPSolver): Unit = cp.add(c)

  def add(c: CPBoolVar)(implicit cp: CPSolver): Unit = cp.add(c)

  def post(c: Constraint, propagStrengh: CPPropagStrength = Weak)(implicit cp: CPSolver): Unit = cp.post(c, propagStrengh)
  def post(c: Constraint)(implicit cp: CPSolver): Unit = cp.post(c)

  def search(branching: Branching)(implicit cp: CPSolver): SearchNode = cp.search(branching)

  def search(block: => Seq[Alternative])(implicit cp: CPSolver): SearchNode = cp.search(block)

  def minimize(obj: CPIntervalVar)(implicit cp: CPSolver): CPSolver = cp.minimize(obj)
  def maximize(obj: CPIntervalVar)(implicit cp: CPSolver): CPSolver = cp.maximize(obj)

  def onSolution(block: => Unit)(implicit cp: CPSolver): SearchNode = cp.onSolution(block)
  def onSolutionWithStats(block: SearchStatistics => Unit)(implicit cp: CPSolver): SearchNode = cp.onSolutionWithStats(block)

  def start(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(implicit cp: CPSolver): SearchStatistics = {
    cp.start(nSols, failureLimit, timeLimit, maxDiscrepancy)
  }

  def startSubjectTo(nSols: Int = Int.MaxValue, failureLimit: Int = Int.MaxValue, timeLimit: Int = Int.MaxValue, maxDiscrepancy: Int = Int.MaxValue)(reversibleBlock: => Unit = {})(implicit cp: CPSolver): SearchStatistics = {
    cp.startSubjectTo(nSols, failureLimit, timeLimit, maxDiscrepancy)(reversibleBlock)
  }
}