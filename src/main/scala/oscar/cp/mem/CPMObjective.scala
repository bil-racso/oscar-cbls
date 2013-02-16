package oscar.cp.mem

import oscar.cp.constraints.CPObjective
import oscar.cp.constraints.CPObjectiveUnit
import oscar.cp.core.Store
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.reversible.ReversiblePointer
import oscar.cp.mem.pareto.SolNode
import oscar.cp.core.CPVarInt
import oscar.cp.mem.pareto.LinkedNode
import oscar.reversible.ReversibleInt
import oscar.cp.modeling.TightenType

class CPMObjective[Sol](st: Store, pareto: ParetoSet[Sol], objs: CPObjectiveUnit*) extends CPObjective(st, objs:_*) {

  val lastBound: Array[ReversiblePointer[LinkedNode[SolNode[Sol]]]] = Array.fill(pareto.nObjs)(new ReversiblePointer(st, null))
  
  override def propagate(): CPOutcome = {
    if (pareto.Objs.forall(o => {
      updateUpperBound(o)     
      objs(o).filter() != CPOutcome.Failure
    })) CPOutcome.Suspend
    else CPOutcome.Failure
  }
  
  private def updateUpperBound(obj: Int) {
    lastBound(obj).setValue(null)
    adjustMin(obj, objs(obj).objVar.min)
    objs(1-obj).best = upperBound(obj, 1-obj)
  }
    
  private def adjustMin(obj: Int, min: Int) {
    while (min >= nextVal(obj)) {
      if (lastBound(obj).value == null) lastBound(obj).setValue(pareto.objsVal(obj).first)
      else lastBound(obj).setValue(lastBound(obj).value.next)
    }
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
    else lastBound(obj).value.solNode.sol(o)
  }

  override def setup(l: CPPropagStrength): CPOutcome = {
    objs.foreach(_.objVar.callPropagateWhenBoundsChange(this))
    objs.foreach(s.post(_))
    propagate()
  }
}