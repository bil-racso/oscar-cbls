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
import oscar.cp.modeling.CPSolver
import oscar.reversible.ReversiblePointer
import oscar.cp.mem.pareto.LinkedNode
import oscar.cp.mem.pareto.SolNode

class DynDominanceConstraint[Sol](cp: Store, pareto: ParetoSet[Sol], objs: CPVarInt*) extends Constraint(cp, "Dominance") {

  val lastBound: Array[ReversiblePointer[LinkedNode[SolNode[Sol]]]] = Array.fill(pareto.nObjs)(new ReversiblePointer(cp, null))
  
  override def propagate(): CPOutcome = {
    if (pareto.Objs.forall(o => updateUpperBound(o) != CPOutcome.Failure)) CPOutcome.Suspend
    else CPOutcome.Failure
  }
  
  private def updateUpperBound(obj: Int): CPOutcome = {
    lastBound(obj).setValue(null)
    adjustMin(obj, objs(obj).min)
    objs(1-obj).updateMax(upperBound(obj, 1-obj) - 1)
  }
    
  private def adjustMin(obj: Int, min: Int) {
    while (min >= nextVal(obj)) {
      if (lastBound(obj).value == null) lastBound(obj).setValue(pareto.objsVal(obj).first)
      else lastBound(obj).setValue(lastBound(obj).value.next)
    }
  }
  
  override def updateMinIdx(cpvar: CPVarInt, obj: Int, v: Int): CPOutcome = {
    return updateUpperBound(obj)
  }
  
  private def nextVal(obj: Int): Int = {
    if (lastBound(obj).value == null) {
      if (pareto.objsVal(obj).isEmpty) pareto.nadir(obj)
      else pareto.objsVal(obj).first.objVal
    }
    else if (lastBound(obj).value.isLast) pareto.nadir(obj)
    else lastBound(obj).value.next.objVal
  }
  
  private def upperBound(obj: Int, o: Int) = {
    if (lastBound(obj).value == null) pareto.nadir(o)
    else lastBound(obj).value.solNode.sol.objs(o)
  }

  override def setup(l: CPPropagStrength): CPOutcome = {
    setIdempotent()
    for (o <- pareto.Objs)
      objs(o).callUpdateMinIdxWhenMinChanges(this, o)
    propagate()
  }
}
