package oscar.cp.lcg.constraints

import oscar.cp.core.Constraint
import oscar.cp.core.CPStore
import oscar.cp.lcg.core.LCGStore
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength

abstract class LCGConstraint(val lcgStore: LCGStore, store: CPStore, name: String) extends Constraint(store, name) {

  def register(): Unit

  def explain(): Unit
  
  final override def propagate(): CPOutcome = {
    explain(); CPOutcome.Suspend
  } 
  
  final override def setup(l: CPPropagStrength): CPOutcome = {
    register(); CPOutcome.Suspend
  }
}