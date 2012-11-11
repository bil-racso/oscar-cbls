package oscar.cp.mem.pareto

/** ParetoPoint
 * 
 *  @author Renaud Hartert - ren.hartert@gmail.com
 */

class ParetoPoint[S](val sol: S, val nodeObj1: LinkedNode[S], val nodeObj2: LinkedNode[S]) {

  private val nObjs = 2
  private val Objs = 0 until 2
  
  def obj1 = nodeObj1.value
  def obj2 = nodeObj2.value

  def apply(i: Int) = if (i == 0) obj1 else obj2
  
  def objNode(obj: Int) = {
    if (obj == 0) nodeObj1 
    else if (obj == 1) nodeObj2
    else throw new NoSuchElementException("biobjective point")
  }

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
    if (obj1 <= point.obj1 && obj2 < point.obj2) true
    else if (obj1 < point.obj1 && obj2 <= point.obj2) true
    else false
  }

  override def toString: String = "(" + obj1 + ", " + obj2 + ")"
}

object ParetoPoint {
  
  def apply[S](sol: S, nodeObj1: LinkedNode[S], nodeObj2: LinkedNode[S]) = new ParetoPoint[S](sol, nodeObj1, nodeObj2)
}
