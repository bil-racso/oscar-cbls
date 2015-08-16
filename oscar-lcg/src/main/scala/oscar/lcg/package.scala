package oscar

import oscar.lcg.core.LCGStore

package object lcg {

  type IntVar = oscar.lcg.variables.IntVar
  final val IntVar = oscar.lcg.variables.IntVar
  
  trait LCGModel { implicit val lcgStore = new LCGStore }
  
  implicit final class IntVarOps(val intVar: IntVar) extends AnyVal {
    
    def value: Int = {
      if (intVar.isAssigned) intVar.min
      else throw new NoSuchElementException(s"${intVar.name} is unassigned.")
    }
  }
}