package oscar.lcg.core

import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome.Failure
import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.heuristic.Heuristic
import oscar.lcg.variables.LCGIntervalVar

/** @author Renaud Hartert ren.hartert@gmail.com */
class LCGSolver {
  
  private[this] val _cpStore: CPStore = new CPStore()
  private[this] val _cdclStore: CDCLStore = new CDCLStore(_cpStore)
  private[this] val _search: LCGSearch = new LCGSearch(_cpStore, _cdclStore)
  
  final val cdclStore: CDCLStore = _cdclStore
  
  final val cpStore: CPStore = _cpStore
  
  final val search: LCGSearch = _search
  
  final def add(constraint: LCGConstraint): Boolean = {
    _cpStore.post(constraint) != Failure
  }
  
  final def add(literal: Literal): Boolean = {
    _cdclStore.enqueue(literal, null)
  }
  
  final def propagate(): Boolean = {
    if (!_cdclStore.propagate()) false
    else _cpStore.propagate() != Failure
  }
  
  final def onSolution(action: => Unit): Unit = {
    _search.onSolution(action)
  }
    
  final def solve(heuristic: Heuristic, stopCondition: => Boolean, resetStatistics: Boolean = true): LiftedBoolean = {
    if (resetStatistics) _search.resetStatistics() // reset statistics of the search
    _search.search(heuristic, () => stopCondition)
  }
  
  /** 
   *  Minimize the objective variable 
   *  Returns True if the best solution is optimal
   *  Returns False if the best solution was not proven to be optimal
   */
  final def minimize(heuristic: Heuristic, objective: LCGIntervalVar, stopCondition: => Boolean): LiftedBoolean = {
    _search.resetStatistics() // reset statistics once for all the minimize process
    var best = objective.max
    var state: LiftedBoolean = Unassigned
    while (state == Unassigned) {
      _cdclStore.enqueue(objective.lowerEqual(best), null)
      val out = _search.search(heuristic, () => stopCondition)
      if (out == True) best -= 1
      else if (out == False) state = True
      else state = False
    }
    state
  }
}