package oscar.cp.mem.pareto

class ParetoMinSet[S] {

  private val sortedPoint = Array.fill(2)(new OrderedLinkedList[S])  
  private var currentPoint : ParetoPoint[S] = null
  
  def currentSol = currentPoint.sol
  def currentVal(obj: Int) = currentPoint(obj)
  def currentUB(obj: Int)  = currentPoint.upperValue(obj)
  def currentLB(obj: Int)  = currentPoint.lowerValue(obj)

  def size: Int = sortedPoint(0).size

  def isEmpty = (size == 0)
  
  def nextSol(obj : Int): Boolean = {
    
    if (currentPoint.objNode(obj).isLast) {
      currentPoint = sortedPoint(obj).first.point
      true
    }
    else {
      currentPoint = currentPoint.objNode(obj).next.point
      false
    }
  }
  
  def bestSol(obj : Int) { currentPoint = sortedPoint(obj).first.point }
  
  def worstSol(obj : Int) { currentPoint = sortedPoint(obj).last.point }

  def insert(objs: (Int, Int), sol: S): Boolean = {
    
    val wasEmpty = isEmpty

    val nodeObj1 = sortedPoint(0) insert objs._1
    val nodeObj2 = sortedPoint(1) insert objs._2

    val newPoint = ParetoPoint(sol, nodeObj1, nodeObj2)
    
    nodeObj1.point = newPoint
    nodeObj2.point = newPoint

    if (wasEmpty) {
      currentPoint = newPoint
      false
    }
    else clean(newPoint)
  }

  private def clean(newPoint: ParetoPoint[S]): Boolean = clean0(newPoint, sortedPoint(0).first, false)
  private def clean0(newPoint: ParetoPoint[S], node: LinkedNode[S], change: Boolean): Boolean = {

    if (node == null) change
    else if (!newPoint.isDominating(node.point)) clean0(newPoint, node.next, change)
    else {
      val nextNode = node.next  
      
      currentPoint = newPoint
      sortedPoint(0).remove(node.point.objNode(0))
      sortedPoint(1).remove(node.point.objNode(1))    
      
      clean0(newPoint, nextNode, true)
    }
  }
  
  def points = {
    val points = Array.fill(size)({
      val p = currentPoint
      nextSol(0)
      p
    })
    points.sortBy(_.obj1)
  }
}

object ParetoMinSet {
  
  def apply[S]() = new ParetoMinSet[S]
}
