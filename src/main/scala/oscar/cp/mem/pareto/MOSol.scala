package oscar.cp.mem.pareto

case class MOSol[Sol](sol: Sol, objVals: Array[Int]) {
  
  private[pareto] var ub = Array.fill(objVals.size)(Int.MaxValue)
  private[pareto] var lb = Array.fill(objVals.size)(Int.MinValue)
  
  private[pareto] var us: Array[Option[MOSol[Sol]]] = null
  private[pareto] var ls: Array[Option[MOSol[Sol]]] = null
  
  def upperSol(obj: Int): Option[MOSol[Sol]] = us(obj)
  def lowerSol(obj: Int): Option[MOSol[Sol]] = ls(obj)
  
  def upperBound(obj: Int): Int = ub(obj)
  def lowerBound(obj: Int): Int = lb(obj)
  
  def apply(obj: Int) = objVals(obj)
 
  def dominates[T](sol: MOSol[T]): Boolean = dominates(sol, 0, false)
  
  private def dominates[T](sol: MOSol[T], o: Int, dominate: Boolean): Boolean = {
    if (o == objVals.size) dominate
    else if (objVals(o) < sol.objVals(o)) dominates(sol, o+1, true)
    else if (objVals(o) == sol.objVals(o)) dominates(sol, o+1, dominate)
    else false
  }
  
  def eDominates[T](e: Int, sol: MOSol[T]): Boolean = eDominates(e, sol, 0, false)
  
  private def eDominates[T](e: Int, sol: MOSol[T], o: Int, dominate: Boolean): Boolean = {
    if (o == objVals.size) dominate
    else if (objVals(o)-e < sol.objVals(o)) eDominates(e, sol, o+1, true)
    else if (objVals(o)-e == sol.objVals(o)) eDominates(e, sol, o+1, dominate)
    else false
  }
  
  def weakDominates[T](sol: MOSol[T]): Boolean = weakDominates(sol, 0)
  
  private def weakDominates[T](sol: MOSol[T], o: Int): Boolean = {
    if (o == objVals.size) true
    else if (objVals(o) <= sol.objVals(o)) weakDominates(sol, o+1)
    else false
  }

  def strongDominates[T](sol: MOSol[T]): Boolean = strongDominates(sol, 0)
  
  private def strongDominates[T](sol: MOSol[T], o: Int): Boolean = {
    if (o == objVals.size) true
    else if (objVals(o) < sol.objVals(o)) strongDominates(sol, o+1)
    else false
  }

  override def toString: String = "MOSol("+objVals.mkString(", ")+")"
}

object MOSol {  
  def apply[Sol](sol: Sol, objVals: Int*) = new MOSol[Sol](sol, objVals.toArray) 
  implicit def MOSolToSol[Sol](s : MOSol[Sol]): Sol = s.sol
}