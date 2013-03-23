package oscar.cp.mem.constraints

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.mem.pareto.Pareto

class Gavanelli02[Sol](pareto: Pareto[Sol], isMax: Array[Boolean], objVars: Array[CPVarInt]) extends Constraint(objVars.head.store, "Gavanelli02 Dominance") {

  // Simplifies code understanding
  type Point = IndexedSeq[Int]
  
  override def propagate(): CPOutcome = {    
    //println("propagate gananelli")
    // List of all solutions
    val allSols = pareto.objectiveSols
    // The DPobj of each objective
    val DPobjs = for(o <- pareto.Objs) yield computeDPobj(o)
    // The best dominant solutions according to each objective
    val bestDoms = getAllBestDominant(DPobjs, allSols)
    
    for (o <- pareto.Objs if bestDoms(o).isDefined) {
        
      val bound = bestDoms(o).get(o)
      
      // objective has to be maximized
      if (isMax(o)) { 
        if (objVars(o).updateMin(bound + 1) == Failure) return Failure
      }
      // objective has to be minimized
      else { 
        if (objVars(o).updateMax(bound - 1) == Failure) return Failure
      }
    }
    
    Suspend
  }  
  
  // Returns the array of solutions such that for each objective i, bestDominant(i) is the solution 
  // that dominates DPObjs(i) with the best value for the objective i.
  private def getAllBestDominant(DPobjs: IndexedSeq[Point], sols: List[Point]): Array[Option[Point]] = {
    
    val bestDom: Array[Option[Point]] = Array.fill(pareto.nObjs)(None)
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
  
  // Compute the point which is the best for all objectives except for the objective obj
  private def computeDPobj(obj: Int): IndexedSeq[Int] = {
    for (o <- pareto.Objs) yield {
      if (o == obj) {
        if (isMax(o)) objVars(o).min
        else objVars(o).max
      }
      else {
        if (isMax(o)) objVars(o).max
        else objVars(o).min
      }
    }
  }

  override def setup(l: CPPropagStrength): CPOutcome = {
    setIdempotent()
    if (propagate() == Failure) Failure
    else {
      for(o <- pareto.Objs if !objVars(o).isBound) {
    	if (isMax(o)) objVars(o).callPropagateWhenMaxChanges(this)
    	else objVars(o).callPropagateWhenMinChanges(this)
      }
      Suspend
    }
  }
}

object Gavanelli02 {  
  def apply[Sol](pareto: Pareto[Sol], isMax: Array[Boolean], objs: Array[CPVarInt]): Gavanelli02[Sol] = new Gavanelli02(pareto, isMax, objs)
}
