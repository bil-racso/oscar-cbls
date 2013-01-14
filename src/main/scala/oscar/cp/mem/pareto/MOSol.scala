package oscar.cp.mem.pareto

case class MOSol[Sol](sol: Sol, objs: Array[Int]) {
  
  // Dynamically adjusted by the pareto front
  var ub = Array.fill(objs.size)(0)
  var lb = Array.fill(objs.size)(0)
  
  def upperBound(obj: Int): Int = ub(obj)
  def lowerBound(obj: Int): Int = lb(obj)

  // True if the point dominates x
  def dominates(x: MOSol[Sol]): Boolean = if (x == null) true else dominates0(x, 0)
  
  private def dominates0(x: MOSol[Sol], i: Int): Boolean = {
    if (i == objs.size) true
    else if (x.objs(i) < objs(i)) false
    else dominates0(x, i+1)
  }

  override def toString: String = "MOSol("+objs.mkString(", ")+")"
}

object MOSol {  
  def apply[Sol](sol: Sol, objs: Int*) = new MOSol[Sol](sol, objs.toArray) 
  implicit def MOSolToSol[Sol](s : MOSol[Sol]): Sol = s.sol
}