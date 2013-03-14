package oscar.cp.mem

import oscar.cp.core.CPVarInt
import oscar.cp.core.Store
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.mem.pareto.Pareto
import oscar.cp.mem.pareto.MOSol

class Gavanelli02[Sol](pareto: Pareto[Sol], objVars: CPVarInt*) extends Constraint(objVars.head.store, "Gavanelli02 Dominance") {
  
  override def propagate(): CPOutcome = {    
    
    // List of all solutions
    val allSols = pareto.toList
    // The DPobj of each objective
    val DPobjs = Array.tabulate(pareto.nObjs)(computeDPobj(_)) 
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
 
  /*override def updateMinIdx(cpVar: CPVarInt, obj: Int, v: Int): CPOutcome = {
    for (o <- pareto.Objs if o != obj) {
      if (adjustUpperBound(o) == Failure) return Failure
    }
    Suspend
  }*/
  
  def getAllBestDominant(DPobjs: Array[Array[Int]], sols: List[MOSol[Sol]]): Array[Option[MOSol[Sol]]] = {
    
    val bestDom: Array[Option[MOSol[Sol]]] = Array.fill(pareto.nObjs)(None)
    val bestObj: Array[Int] = Array.fill(pareto.nObjs)(Int.MaxValue)
    
    val dummySols = Array.tabulate(pareto.nObjs)(o => MOSol(null, DPobjs(o)))
    
    for (s <- sols; o <- pareto.Objs) {
      if (s dominates dummySols(o)) {
        if (s(o) < bestObj(o)) {
          bestDom(o) = Some(s)
          bestObj(o) = s(o)
        }
      }
    }
    bestDom
  }
  
  // Update the bounds of obj until DPobj is no more dominated
  /*private def adjustUpperBound(obj: Int): CPOutcome = {  
    val DPobj = computeDPobj(obj)
    var dominant = pareto.getDominant(DPobj)
    while(dominant.isDefined) { // Not really efficient !
      val ub = dominant.get.objVals(obj) - 1 // the solution has to be better
      if (objVars(obj).updateMax(ub) == Failure) return Failure
      DPobj(obj) = ub
      dominant = pareto.getDominant(DPobj)
    }   
    Suspend
  }*/
  
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
        if (!objVars(o).isBound) objVars(o).callPropagateWhenMinChanges(this)
        //if (!objVars(o).isBound) objVars(o).callUpdateMinIdxWhenMinChanges(this, o)
      }
      Suspend
    }
  }
}

object Gavanelli02 {  
  def apply[Sol](pareto: Pareto[Sol], objs: CPVarInt*): Gavanelli02[Sol] = new Gavanelli02(pareto, objs:_*)
}
