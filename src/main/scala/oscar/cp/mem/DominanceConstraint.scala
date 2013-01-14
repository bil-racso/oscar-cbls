package oscar.cp.mem

import oscar.reversible.ReversibleInt
import oscar.cp.core.CPVarInt
import oscar.cp.core.Store
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.mem.pareto.ParetoSet
import oscar.reversible.ReversibleSetIndexedArray
import oscar.cp.mem.pareto.MOSol

class DominanceConstraint[Sol](cp: Store, pareto: ParetoSet[Sol], objs: CPVarInt*) extends Constraint(cp, "Dominance") {

  val nObjs = pareto.nObjs
  val Objs = 0 until nObjs

  val sortedSol: Array[Array[MOSol[Sol]]] = Array.tabulate(nObjs)(o => pareto.sortedByObj(o).toArray) 
  val prevVals: Array[ReversibleInt] = Array.fill(nObjs)(new ReversibleInt(cp, 0))
  
  override def setup(l: CPPropagStrength): CPOutcome = {

    Suspend
  }
  
  private def setNext(obj: Int, value: Int) {
    while (value < sortedSol(obj)(prevVals(obj).getValue + 1).objs(obj)) {
      prevVals(obj).setValue(prevVals(obj).getValue + 1)
    }
  }
  
  private def adjustMin(obj: Int, value: Int) {
    while (value > nextVal(obj)) 
      prevVals(obj).setValue(prevVals(obj).getValue + 1)
  }
  
  private def nextVal(obj: Int): Int = {
    if (prevVals(obj).getValue + 1 == pareto.size) pareto.nadir(obj)
    else sortedSol(obj)(prevVals(obj).getValue + 1).objs(obj)
  }
  
  override def updateMinIdx(cpvar: CPVarInt, obj: Int, newMin: Int): CPOutcome = {
    
    if (newMin > nextVal(obj)) {
      adjustMin(obj, newMin)
      for (o <- Objs if o != obj) {
        if (objs(o).updateMax(sortedSol(obj)(prevVals(obj).getValue).objs(o)) == Failure) return Failure
      }
    }
    Suspend
  }
}