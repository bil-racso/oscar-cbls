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

class DominanceConstraint[Sol](cp: Store, pareto: ParetoSet[Sol], objs: CPVarInt*) extends Constraint(cp, "Dominance") {

  val nObjs = pareto.nObjs
  val Objs = 0 until nObjs

  val sortedSol: Array[Array[MOSol[Sol]]] = Array.tabulate(nObjs)(o => pareto.sortedByObj(o).toArray) 
  val index: Array[ReversibleInt] = Array.fill(nObjs)(new ReversibleInt(cp, 0))
  
  override def setup(l: CPPropagStrength): CPOutcome = {

    for (o <- Objs) {
      if (updateMinIdx(objs(o), o, objs(o).min) == Failure) return Failure
      if (!objs(o).isBound) objs(o).callUpdateMinIdxWhenMinChanges(this, o)
    }
    
    Suspend
  }
  
  private def adjustMin(obj: Int, value: Int) {
    while (value >= nextVal(obj)) 
      index(obj).setValue(index(obj).getValue + 1)
  }
  
  private def nextVal(obj: Int): Int = {
    if (index(obj).getValue == pareto.size) pareto.nadir(obj)
    else sortedSol(obj)(index(obj).getValue).objs(obj)
  }
  
  private def upperBound(obj: Int, o: Int) = {
    if (index(obj).getValue == 0) pareto.nadir(o)
    else sortedSol(obj)(index(obj).getValue - 1).objs(o)
  }
  
  override def updateMinIdx(cpvar: CPVarInt, obj: Int, v: Int): CPOutcome = {
    val newMin = cpvar.min
    if (newMin >= nextVal(obj)) {
      adjustMin(obj, newMin)
      for (o <- Objs if o != obj)
      if (objs(o).updateMax(upperBound(obj, o) - 1) == Failure) return Failure
    }
    return Suspend
  }
}

object DominanceConstraint {
  
  def main(args: Array[String]) {
    
    
    val cp = CPSolver()
    
    val pareto = ParetoSet[String](2)
    
    val s1 = MOSol[String]("sol1", 2, 5)
    pareto.insert(s1)
    val s2 = MOSol[String]("sol2", 5, 2)
    pareto.insert(s2)
    val s3 = MOSol[String]("sol3", 3, 3)
    pareto.insert(s3)
    
    val obj1 = CPVarInt(cp, 0 to 10)
    val obj2 = CPVarInt(cp, 0 to 10)
    
    val constraint = new DominanceConstraint(cp, pareto, obj1, obj2)
    cp.add(constraint)
    
    cp.add(obj1 <= 5)
    cp.add(obj1 >= 2)
    cp.add(obj1 >= 3)
    cp.add(obj1 >= 4)
    cp.add(obj1 >= 5)
    
  }
  
}
