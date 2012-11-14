package oscar.cp.mem.pareto

/**
 * ParetoPoint
 *
 *  @author Renaud Hartert - ren.hartert@gmail.com
 */

class ParetoPoint[S](val sol: S, val nodeObj1: LinkedNode[ParetoPoint[S]], val nodeObj2: LinkedNode[ParetoPoint[S]]) {

  private val nObjs = 2
  private val Objs = 0 until 2

  def obj1 = nodeObj1.key
  def obj2 = nodeObj2.key

  def apply(i: Int) = if (i == 0) obj1 else obj2

  def objNode(obj: Int) = {
    if (obj == 0) nodeObj1
    else if (obj == 1) nodeObj2
    else throw new NoSuchElementException("biobjective point")
  }

  def upperValue(obj: Int) = {
    val node = objNode(obj)
    if (!node.isLast) node.next.key
    else Int.MaxValue
  }

  def lowerValue(obj: Int) = {
    val node = objNode(obj)
    if (!node.isFirst) node.prev.key
    else Int.MinValue
  }

  def isDominating(point: ParetoPoint[S]): Boolean = {
    if (obj1 <= point.obj1 && obj2 < point.obj2) true
    else if (obj1 < point.obj1 && obj2 <= point.obj2) true
    else false
  }

  def divSurf = {
    if (nodeObj1.prev == null || nodeObj1.next == null) 1
    else {
      val x1 = obj1 - nodeObj1.prev.key
      val y1 = nodeObj2.next.key - obj2
      val x2 = nodeObj1.next.key - obj1
      val y2 = obj2 - nodeObj2.prev.key

      x1 * y1 + x2 * y2
    }
  }

  def intSurf = {
    val x = obj1 - nodeObj1.prev.key
    val y = obj2 - nodeObj2.prev.key

    x * y
  }

  override def toString: String = "(" + obj1 + ", " + obj2 + ")"
}

object ParetoPoint {

  def apply[S](sol: S, nodeObj1: LinkedNode[ParetoPoint[S]], nodeObj2: LinkedNode[ParetoPoint[S]]) = new ParetoPoint[S](sol, nodeObj1, nodeObj2)
}
