package oscar.cp.mem

import oscar.reversible.ReversibleInt
import oscar.cp.core.CPVarInt
import oscar.cp.core.Store
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.mem.pareto.Pareto
import oscar.reversible.ReversibleSetIndexedArray
import oscar.cp.mem.pareto.MOSol
import oscar.cp.modeling.CPSolver
import oscar.reversible.ReversiblePointer
import oscar.cp.mem.pareto.LinkedNode
import oscar.cp.mem.pareto.SolNode

class Gavanelli02[Sol](cp: Store, pareto: Pareto[Sol], objVars: CPVarInt*) extends Constraint(cp, "Gavanelli02 Dominance") {

  override def propagate(): CPOutcome = {
    for (o <- pareto.Objs) {
      if (adjustUpperBound(o) == Failure) return Failure
    }
    Suspend
  }
 
  override def updateMinIdx(cpvar: CPVarInt, obj: Int, v: Int): CPOutcome = {
    for (o <- pareto.Objs if o != obj) {
      if (adjustUpperBound(obj) == Failure) return Failure
    }
    Suspend
  }
  
  // Update the bounds of obj until DPobj is no more dominated
  private def adjustUpperBound(obj: Int): CPOutcome = {  
    val DPobj = computeDPobj(obj)
    var dominant = pareto.getDominant(DPobj)
    while(dominant.isDefined) {
      val ub = dominant.get.objs(obj) - 1 // the solution has to be better
      if (objVars(obj).updateMax(ub) == Failure) return Failure
      DPobj(obj) = ub
      dominant = pareto.getDominant(DPobj)
    }   
    Suspend
  }
  
  // Compute the point which is the best for all objectives except for objective obj
  private def computeDPobj(obj: Int): Array[Int] = {
    Array.tabulate(pareto.nObjs)(o => {
      if (o == obj) objVars(o).max
      else objVars(o).min
    })
  }

  override def setup(l: CPPropagStrength): CPOutcome = {
    setIdempotent()
    if (propagate() == Failure) Failure
    else {
      for(o <- pareto.Objs) {
        if (!objVars(o).isBound) objVars(o).callUpdateMinIdxWhenMinChanges(this, o)
      }
      Suspend
    }
  }
}
