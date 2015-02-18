package oscar.lcg.core

import oscar.cp.core.CPStore
import oscar.cp.core.CPOutcome.Failure
import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.heuristic.Heuristic

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

  final def solve(heuristic: Heuristic, stopCondition: => Boolean): LiftedBoolean = {
    _search.search(heuristic, () => stopCondition)
  }
  
  final def onSolution(action: => Unit): Unit = {
    _search.onSolution(action)
  }
}