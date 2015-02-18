package oscar

import oscar.lcg.variables.LCGIntervalViewOffset
import oscar.lcg.constraints.LCGConstraint
import oscar.lcg.constraints.LowerEqual
import oscar.lcg.core.Literal

/** @author Renaud Hartert ren.hartert@gmail.com */
package object lcg {

  type LCGIntervalVar = oscar.lcg.variables.LCGIntervalVar
  final val LCGIntervalVar = oscar.lcg.variables.LCGIntervalVar
  
  type CPSolver = oscar.cp.core.CPSolver
  final val CPSolver = oscar.cp.core.CPSolver
  
  type CDCLStore = oscar.lcg.core.CDCLStore
  //final val LCGStore = oscar.cp.lcg.core.LCGStore
  
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
    
    final def <= (that: LCGIntervalVar): LCGConstraint = new LowerEqual(variable, that)
  }
  
  final def add(constraint: LCGConstraint)(implicit lcgSolver: LCGSolver): Boolean = lcgSolver.add(constraint)
  
  final def add(literal: Literal)(implicit lcgSolver: LCGSolver): Boolean = lcgSolver.add(literal)
  
  final def onSolution(action: => Unit)(implicit lcgSolver: LCGSolver): Unit = lcgSolver.onSolution(action)

  final def solve(heuristic: Heuristic, stopCondition: => Boolean)(implicit lcgSolver: LCGSolver): LiftedBoolean = lcgSolver.solve(heuristic, stopCondition)
  final def solve(heuristic: Heuristic)(implicit lcgSolver: LCGSolver): LiftedBoolean = lcgSolver.solve(heuristic, false)
}