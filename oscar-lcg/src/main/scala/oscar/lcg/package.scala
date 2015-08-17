package oscar

import oscar.lcg.core.LCGStore
import oscar.lcg.core.Constraint
import oscar.lcg.searches.Heuristic
import oscar.lcg.searches.DFSearch
import oscar.lcg.modeling.Heuristics

package object lcg extends Heuristics {

  type IntVar = oscar.lcg.variables.IntVar
  final val IntVar = oscar.lcg.variables.IntVar
  
  type BooleanVar = oscar.lcg.variables.BooleanVar
  final val BooleanVar = oscar.lcg.variables.BooleanVar
  
  trait LCGModel { 
    implicit val lcgStore = new LCGStore 
    implicit val dfsearch = new DFSearch(lcgStore)
  }
  
  implicit final class IntVarOps(val intVar: IntVar) extends AnyVal {    
    def value: Int = {
      if (intVar.isAssigned) intVar.min
      else throw new NoSuchElementException(s"${intVar.name} is unassigned.")
    }
  }
  
  implicit final class BooleanVarOps(val booleanVar: BooleanVar) extends AnyVal {    
    def value: Boolean = {
      if (booleanVar.isAssigned) booleanVar.isTrue
      else throw new NoSuchElementException(s"${booleanVar.name} is unassigned.")
    }
  }
  
  final def add(constraint: Constraint)(implicit lcgStore: LCGStore): Boolean = {
    if (!constraint.setup()) false
    else if (!lcgStore.propagate()) false
    else true
  }
  
  final def search(heuristic: Heuristic, stopCondition: => Boolean)(implicit dfsearch: DFSearch): Unit = {
    dfsearch.start(heuristic, s => stopCondition)
  }
  
  final def search(heuristic: Heuristic)(implicit dfsearch: DFSearch): Unit = {
    dfsearch.start(heuristic, s => false)
  }
  
  final def onSolution(action: => Unit)(implicit dfsearch: DFSearch): Unit = {
    dfsearch.onSolution(action)
  }
}