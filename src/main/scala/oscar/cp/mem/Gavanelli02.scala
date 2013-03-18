package oscar.cp.mem

import oscar.cp.core.CPVarInt
import oscar.cp.core.Store
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.mem.pareto.Pareto

class Gavanelli02[Sol](pareto: Pareto[Sol], objMax: Array[Boolean], objVars: Array[CPVarInt]) extends Constraint(objVars.head.store, "Gavanelli02 Dominance") {
  
  override def propagate(): CPOutcome = {    
    
    // List of all solutions
    val allSols = pareto.objectiveSols
    // The DPobj of each objective
    val DPobjs = for(o <- pareto.Objs) yield computeDPobj(o)
    // The best dominant solutions according to each objective
    val bestDoms = getAllBestDominant(DPobjs, allSols)
    
    for (o <- pareto.Objs) {
      if (bestDoms(o).isDefined) {
        val ub = bestDoms(o).get(o) - 1
        if (objVars(o).updateMax(ub) == Failure) return Failure
      }
    }
    
    Suspend
  }  
  
  def getAllBestDominant(DPobjs: IndexedSeq[IndexedSeq[Int]], sols: List[IndexedSeq[Int]]): Array[Option[IndexedSeq[Int]]] = {
    
    val bestDom: Array[Option[IndexedSeq[Int]]] = Array.fill(pareto.nObjs)(None)
    val bestObj: Array[Int] = Array.fill(pareto.nObjs)(Int.MaxValue)
    
    for (s <- sols; o <- pareto.Objs) {
      if (pareto.dominate(s, DPobjs(o))) {
        if (pareto.isBetter(o)(s(o), bestObj(o))) {
          bestDom(o) = Some(s)
          bestObj(o) = s(o)
        }
      }
    }
    bestDom
  }
  
  // Compute the point which is the best for all objectives except for objective obj
  private def computeDPobj(obj: Int): IndexedSeq[Int] = {
    for (o <- pareto.Objs) yield {
      if (o == obj) objVars(o).max
      else objVars(o).min
    }
  }

  override def setup(l: CPPropagStrength): CPOutcome = {
    setIdempotent()
    if (propagate() == Failure) Failure
    else {
      for(o <- pareto.Objs if !objVars(o).isBound) {
    	if (objMax(o)) objVars(o).callPropagateWhenMaxChanges(this)
    	else objVars(o).callPropagateWhenMinChanges(this)
      }
      Suspend
    }
  }
}

object Gavanelli02 {  
  def apply[Sol](pareto: Pareto[Sol], objMax: Array[Boolean], objs: Array[CPVarInt]): Gavanelli02[Sol] = new Gavanelli02(pareto, objMax, objs)
}
