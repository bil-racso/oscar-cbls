package oscar.cp.lcg.constraints

import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.cp.lcg.core.LCGStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.lcg.core.LCGSolver

abstract class LCGConstraint(val lcgStore: LCGSolver, store: CPStore, name: String) extends Constraint(store, name) {

  def register(): Unit

  def explain(): CPOutcome
  
  final override def propagate(): CPOutcome = {
    explain()
  } 
  
  final override def setup(l: CPPropagStrength): CPOutcome = {
    register(); CPOutcome.Suspend
  }
}