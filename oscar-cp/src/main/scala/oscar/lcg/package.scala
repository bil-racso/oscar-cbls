package oscar

package object lcg {

  type LCGIntervalVar = oscar.lcg.variables.LCGIntervalVar
  final val LCGIntervalVar = oscar.lcg.variables.LCGIntervalVar
  
  type CPSolver = oscar.cp.core.CPSolver
  final val CPSolver = oscar.cp.core.CPSolver
  
  type CDCLStore = oscar.lcg.core.CDCLStore
  //final val LCGStore = oscar.cp.lcg.core.LCGStore
  
  trait LCGModel { 
    implicit val cpSolver: CPSolver = CPSolver() 
    implicit val lcgStore: CDCLStore = new CDCLStore(cpSolver)
  }
}