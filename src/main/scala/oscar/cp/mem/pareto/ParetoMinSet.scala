package oscar.cp.mem.pareto

class ParetoMinSet[S](val nObjs: Int) {

  private val sortedPoint = Array.fill(nObjs)(new OrderedLinkedList[S])
  private val currentPoint: Array[ParetoPoint[S]] = Array.fill(nObjs)(null)

  def size: Int = sortedPoint(0).size

  def isEmpty = (size == 0)
  
  def nextSol(obj : Int): S = {
    
    if (currentPoint(obj).objNode(obj).isLast) currentPoint(obj) = sortedPoint(obj).first.point
    else currentPoint(obj) = currentPoint(obj).objNode(obj).next.point
    
    currentPoint(obj).sol
  }
  
  def bestSol(obj : Int): S = {
    currentPoint(obj) = sortedPoint(obj).first.point
    currentPoint(obj).sol
  }
  
  def worstSol(obj : Int): S = {
    currentPoint(obj) = sortedPoint(obj).last.point
    currentPoint(obj).sol
  }

  def insert(objs: (Int, Int), sol: S): Boolean = {
    
    val wasEmpty = isEmpty

    val nodeObj1 = sortedPoint(0) insert objs._1
    val nodeObj2 = sortedPoint(1) insert objs._2

    val newPoint = ParetoPoint(sol, nodeObj1, nodeObj2)
    nodeObj1.point = newPoint
    nodeObj2.point = newPoint

    if (wasEmpty) {
      currentPoint(0) = newPoint
      currentPoint(1) = newPoint
      false
    }
    else clean(newPoint)
  }

  private def clean(newPoint: ParetoPoint[S]): Boolean = clean0(newPoint, sortedPoint(0).first, false)
  private def clean0(newPoint: ParetoPoint[S], node: LinkedNode[S], change: Boolean): Boolean = {

    if (node == null) change
    else if (newPoint isDominating node.point) {
      val nextNode = node.next     
      removePoint(node.point, newPoint)      
      clean0(newPoint, nextNode, true)
    } else clean0(newPoint, node.next, change)
  }

  def removePoint(point: ParetoPoint[S], newPoint: ParetoPoint[S]) {
    if (point == currentPoint(0)) currentPoint(0) = newPoint
    else currentPoint(1) = newPoint    
    sortedPoint(0).remove(point.objNode(0))
    sortedPoint(1).remove(point.objNode(1))
  }
}
