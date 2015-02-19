package oscar

import oscar.lcg.modeling.Heuristics
import oscar.lcg.modeling.Constraints
import oscar.lcg.modeling.SolverUtils
import oscar.lcg.variables.LCGIntervalViewOffset
import oscar.lcg.constraints.LCGConstraint

/** @author Renaud Hartert ren.hartert@gmail.com */
package object lcg extends Heuristics with Constraints with SolverUtils {
  
  type Literal = oscar.lcg.core.Literal

  type LCGIntervalVar = oscar.lcg.variables.LCGIntervalVar
  final val LCGIntervalVar = oscar.lcg.variables.LCGIntervalVar
  
  type CPSolver = oscar.cp.core.CPSolver
  final val CPSolver = oscar.cp.core.CPSolver
  
  type CDCLStore = oscar.lcg.core.CDCLStore
  
  type LCGSolver = oscar.lcg.core.LCGSolver
  
  type Heuristic = oscar.lcg.heuristic.Heuristic
  
  type LiftedBoolean = oscar.lcg.core.LiftedBoolean
  final val Unassigned = oscar.lcg.core.Unassigned
  final val False = oscar.lcg.core.False
  final val True = oscar.lcg.core.True
  
  trait LCGModel { implicit val lcgSolver: LCGSolver = new LCGSolver }
  
  implicit class LCGIntervalVarOps(val variable: LCGIntervalVar) extends AnyVal {
    
    final def + (that: Int): LCGIntervalVar = new LCGIntervalViewOffset(variable, that, s"[${variable.name} + $that]")
    
    final def - (that: Int): LCGIntervalVar = new LCGIntervalViewOffset(variable, -that, s"[${variable.name} - $that]")
    
    final def <= (that: Int): Literal = variable.lowerEqual(that) 
      
    final def < (that: Int): Literal = variable.lowerEqual(that - 1)
      
    final def >= (that: Int): Literal = variable.greaterEqual(that)
      
    final def > (that: Int): Literal = variable.greaterEqual(that + 1)
    
    final def <= (that: LCGIntervalVar): LCGConstraint = lowerEqual(variable, that)
  }
}