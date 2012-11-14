package oscar.cp.mem.pareto

class ParetoMinSet[S] {

  private val objVals = Array.fill(2)(new OrderedLinkedList[ParetoPoint[S]])  
  
  private val divSurf = new OrderedLinkedList[ParetoPoint[S]]
  private val intSurf = new OrderedLinkedList[ParetoPoint[S]]
  
  private var currentPoint : ParetoPoint[S] = null
  
  def currentSol = currentPoint.sol
  def currentVal(obj: Int) = currentPoint(obj)
  def currentUB(obj: Int)  = currentPoint.upperValue(obj)
  def currentLB(obj: Int)  = currentPoint.lowerValue(obj)

  def size = objVals(0).size
  def isEmpty = (size == 0)
  
  def nextSol(obj : Int): Boolean = {
    
    if (currentPoint.objNode(obj).isLast) {
      currentPoint = objVals(obj).first.value
      true
    }
    else {
      currentPoint = currentPoint.objNode(obj).next.value
      false
    }
  }
  
  def bestSol(obj : Int) { currentPoint = objVals(obj).first.value }
  
  def worstSol(obj : Int) { currentPoint = objVals(obj).last.value }

  def insert(objs: (Int, Int), sol: S): Boolean = {
    
    val wasEmpty = isEmpty

    val nodeObj1 = objVals(0) insert (objs._1, null)
    val nodeObj2 = objVals(1) insert (objs._2, null)

    val newPoint = ParetoPoint[S](sol, nodeObj1, nodeObj2)
    
    nodeObj1.value = newPoint
    nodeObj2.value = newPoint

    if (wasEmpty) {
      currentPoint = newPoint
      false
    }
    else clean(newPoint)
  }

  private def clean(newPoint: ParetoPoint[S]): Boolean = clean0(newPoint, objVals(0).first, false)
  private def clean0(newPoint: ParetoPoint[S], node: LinkedNode[ParetoPoint[S]], change: Boolean): Boolean = {

    if (node == null) change
    else if (!newPoint.isDominating(node.value)) clean0(newPoint, node.next, change)
    else {
      val nextNode = node.next  
      
      currentPoint = newPoint
      objVals(0).remove(node.value.objNode(0))
      objVals(1).remove(node.value.objNode(1))    
      
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
  
  def bestDivSurf = {
    val points = Array.fill(size)({
      val p = currentPoint
      nextSol(0)
      p
    }).sortBy(-_.divSurf)
    
    currentPoint = points(0)
  }
}

object ParetoMinSet {
  
  def apply[S]() = new ParetoMinSet[S]
}
