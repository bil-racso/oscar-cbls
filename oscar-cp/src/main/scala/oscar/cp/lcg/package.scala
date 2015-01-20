package oscar.cp

package object lcg {
    
  type LCGIntervalVar = oscar.cp.lcg.variables.LCGIntervalVar
  final val LCGIntervalVar = oscar.cp.lcg.variables.LCGIntervalVar
  
  type CPSolver = oscar.cp.core.CPSolver
  final val CPSolver = oscar.cp.core.CPSolver
  
  type LCGStore = oscar.cp.lcg.core.LCGStore
  //final val LCGStore = oscar.cp.lcg.core.LCGStore
  
  trait LCGModel { 
    implicit val cpSolver: CPSolver = CPSolver() 
    implicit val lcgSolver: LCGStore = new LCGStore(cpSolver)
    // Add the LCG constraint to the CP solver
  }
}