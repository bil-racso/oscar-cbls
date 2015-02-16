package oscar.lcg.core

import oscar.cp.core.CPStore
import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.heuristic.Heuristic

class LCGSolver {
  
  private[this] val _cpStore: CPStore = ???
  private[this] val _cdclStore: CDCLStore = ???
  private[this] val _search: LCGSearch = ???
  
  def add(constraint: LCGConstraint): LiftedBoolean = ???
  
  def cdclStore: CDCLStore = ???
  
  def cpStore: CPStore = ???
  
  def search: LCGSearch = ???
  
  def start(heuristic: Heuristic, stopCondition: => Boolean): LiftedBoolean = ???

}