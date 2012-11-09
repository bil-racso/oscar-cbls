package oscar.cp.mem.pareto

class ParetoPoint[S](val sol: S, val nodeObj1: LinkedNode[S], val nodeObj2: LinkedNode[S]) {

  private val nObjs = 2
  private val Objs = 0 until 2
  
  def obj1 = nodeObj1.value
  def obj2 = nodeObj2.value

  def apply(i: Int) = if (i == 0) obj1 else obj2
  
  private def objNode(obj: Int) = if (obj == 0) nodeObj1 else nodeObj2

  def upperValue(obj: Int) = {
    val node = objNode(obj)
    if (!node.isLast) node.next.value
    else Int.MaxValue
  }

  def lowerValue(obj: Int) = {
    val node = objNode(obj)
    if (!node.isFirst) node.prev.value
    else Int.MinValue
  }

  def isDominating(point: ParetoPoint[S]): Boolean = {
    if (obj1 < point.obj1) false
    else if (obj2 < point.obj2) false
    else true
  }

  override def toString: String = "[" + obj1 + ", " + obj2 + "]"
}

object ParetoPoint {
  
  def apply[S](sol: S, nodeObj1: LinkedNode[S], nodeObj2: LinkedNode[S]) = new ParetoPoint[S](sol, nodeObj1, nodeObj2)
}
