package oscar.cp.mem.pareto

case class MOSol[Sol](sol: Sol, objs: Array[Int]) {
  
  private[pareto] var ub = Array.fill(objs.size)(Int.MaxValue)
  private[pareto] var lb = Array.fill(objs.size)(Int.MinValue)
  
  def upperBound(obj: Int): Int = ub(obj)
  def lowerBound(obj: Int): Int = lb(obj)
  
  private[pareto] var us: Array[Option[MOSol[Sol]]] = null
  private[pareto] var ls: Array[Option[MOSol[Sol]]] = null
  
  def upperSol(obj: Int): Option[MOSol[Sol]] = us(obj)
  def lowerSol(obj: Int): Option[MOSol[Sol]] = ls(obj)

  // True if the point dominates x
  def dominates[T](x: MOSol[T]): Boolean = {
    if (x == null) true 
    else {
      var dominates = true
      for (o <- 0 until objs.size if x.objs(o) < objs(o)) return false
      return true
    }
  }

  override def toString: String = "MOSol("+objs.mkString(", ")+")"
}

object MOSol {  
  def apply[Sol](sol: Sol, objs: Int*) = new MOSol[Sol](sol, objs.toArray) 
  implicit def MOSolToSol[Sol](s : MOSol[Sol]): Sol = s.sol
}