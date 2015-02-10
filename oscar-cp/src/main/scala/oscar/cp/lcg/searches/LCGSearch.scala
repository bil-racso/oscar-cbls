package oscar.cp.lcg.searches

import oscar.algo.search.DFSearchNode
import oscar.algo.search.DFSearch
import oscar.cp.lcg.core.LCGStore
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome.Failure
import oscar.cp.lcg.core.LCGSolver

/** @author Renaud Hartert ren.hartert@gmail.com */
class LCGSearch(node: DFSearchNode, cpStore: CPStore, lcgStore: LCGStore) {

  // LCG constraint
  private[this] val lcgStoreConstraint: Constraint = new LCGSolver(cpStore, lcgStore)

  // Current depth level
  private[this] var depth: Int = 0

  // Number of backtracks of the previous search
  private[this] var nbBkts: Int = 0

  // Number of solutions of the previous search
  private[this] var nbSols: Int = 0

  // Number of nodes explored in the previous search
  private[this] var nbNodes: Int = 0

  // True if the previous search was exhaustive
  private[this] var completed: Boolean = false

  // Actions to execute in case of solution node
  private[this] var solutionActions = List.empty[() => Unit]

  // Actions to execute in case of failed node
  private[this] var failureActions = List.empty[() => Unit]

  /** Returns the number of backtracks in the previous search */
  final def nBacktracks: Int = nbBkts

  /** Returns the number of solutions found in the previous search */
  final def nSolutions: Int = nbSols

  /** Returns the number nodes explored in the previous search */
  final def nNodes: Int = nbNodes

  /** Returns true if the previous search was exhaustive */
  final def isCompleted: Boolean = completed

  /** Adds an action to execute when a failed node is found */
  final def onFailure(action: => Unit): Unit = failureActions = (() => action) :: failureActions

  /** Adds an action to execute when a solution node is found */
  final def onSolution(action: => Unit): Unit = solutionActions = (() => action) :: solutionActions

  /** Clear all actions executed when a solution node is found */
  final def clearOnSolution(): Unit = solutionActions = Nil

  /** Clear all actions executed when a failed node is found */
  final def clearOnFailure(): Unit = failureActions = Nil

  final def search(heuristic: Heuristic, stopCondition: () => Boolean): Unit = {

    // Init
    nbSols = 0
    nbBkts = 0
    nbNodes = 0
    depth = 0
    completed = false
    node.pushState()

    while (depth >= 0 && !stopCondition()) {
      // Next decision to apply
      val decision = heuristic.decision
      if (decision == null) {
        // Solution
        node.solFound()
        solutionActions.foreach(_())
        nbSols += 1
        depth -= 1 // backtrack
        node.pop()
      } else {
        // Expand
        nbNodes += 1
        depth += 1
        node.pushState()
        // Apply decision
        lcgStore.newLevel()
        decision()
        val failed = cpStore.propagate(lcgStoreConstraint)
        if (failed == Failure) {
          failureActions.foreach(_())
          val level = lcgStore.backtrackLvl
          println("backjump from " + depth + " to " + level)
          nbBkts += 1
          while (depth > level) {
            depth -= 1 // backtrack
            node.pop
          }
          // Open the last state
          node.resetLastState()
        }
      }
    }

    // Pop the remaining states
    if (depth < 0) completed = true
    else while (depth > 0) {
      node.pop
      depth -= 1
    }
  }
}