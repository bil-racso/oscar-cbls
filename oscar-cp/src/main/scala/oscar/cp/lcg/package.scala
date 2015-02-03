package oscar.cp

import oscar.cp.lcg.core.LCGSolver
package object lcg {
    
  type LCGIntervalVar = oscar.cp.lcg.variables.LCGIntervalVar
  final val LCGIntervalVar = oscar.cp.lcg.variables.LCGIntervalVar
  
  type CPSolver = oscar.cp.core.CPSolver
  final val CPSolver = oscar.cp.core.CPSolver
  
  type LCGStore = oscar.cp.lcg.core.LCGStore
  //final val LCGStore = oscar.cp.lcg.core.LCGStore
  
  trait LCGModel { 
    implicit val cpSolver: CPSolver = CPSolver() 
    final val lcgStore: LCGStore = new LCGStore(cpSolver)
    implicit val lcgSolver: LCGSolver = new LCGSolver(cpSolver, lcgStore)
    cpSolver.addCut(lcgSolver)
  }
}