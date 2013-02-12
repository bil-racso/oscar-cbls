package oscar.cp.mem.pareto

class RBPareto[Sol] {

  val nObjs = 2
  val Objs = 0 until nObjs
  
  var nadir: Array[Int] = Array.fill(nObjs)(Int.MaxValue)
  var ideal: Array[Int] = Array.fill(nObjs)(0)
  
  val sols: RBLinkedTree[MOSol[Sol]] = new RBLinkedTree()
  
  private def updateBounds(node: sols.RBNode) {
    
    if(node.hasPrev) {
      node.prev.value.ub(0) = node.value.objs(0)
      node.value.lb(0) = node.prev.value.objs(0)
      node.prev.value.lb(1) = node.value.objs(1)
      node.value.ub(1) = node.prev.value.objs(1)
    }
    else {
      node.value.lb(0) = ideal(0)
      node.value.ub(1) = nadir(1)
    }
    
    if(node.hasNext) {
      node.next.value.lb(0) = node.value.objs(0)
      node.value.ub(0) = node.next.value.objs(0)
      node.next.value.ub(1) = node.value.objs(1)
      node.value.lb(1) = node.next.value.objs(1)
    }
    else {
      node.value.ub(0) = nadir(0)
      node.value.lb(1) = ideal(1)
    }
  }
  
  private def clean(sol: MOSol[Sol], node: sols.RBNode, n: Int) : Int = {
    if (!sol.dominates(node.value)) n
    else {
      removeSol(node)
      if (node.hasNext) clean(sol, node.next, n+1)
      else n+1
    }
  }
  
  private def removeSol(node: sols.RBNode) {   
    if (node.hasPrev) {
      node.prev.value.ub(0) = if (node.hasNext) node.next.value.objs(0) else nadir(0)
      node.prev.value.lb(1) = if (node.hasNext) node.next.value.objs(1) else ideal(1)
    }
    if (node.hasNext) {
      node.next.value.lb(0) = if (node.hasPrev) node.next.value.objs(0) else ideal(0)
      node.next.value.ub(1) = if (node.hasPrev) node.next.value.objs(1) else ideal(1) 
    }
    sols.remove(node)
  }

  def insert(sol: MOSol[Sol]): Int = {

    // Insert solution
    if (sols.isEmpty) {      
      val newNode = sols.insert(sol.objs(0), sol)
      updateBounds(newNode)
      0
    } 
    // Check dominance
    else {          
      val closestNode = sols.find(sol.objs(0))
      val n = checkNode(sol, closestNode)
      if (n != 0) n
      else {
        val newNode = sols.insertIn(sol.objs(0), sol, closestNode)
        updateBounds(newNode)
        0
      }
    }
  }
  
  private def checkNode(sol: MOSol[Sol], node: sols.RBNode): Int = {   
    if (node.value.objs(0) <= sol.objs(0)) checkLeft(sol, node)
    else checkRight(sol, node)
  }
  
  private def checkLeft(sol: MOSol[Sol], node: sols.RBNode): Int = {      
    // Dominating Quadrant
    if (node.value.objs(1) <= sol.objs(1)) -1
    // Dominated Quadrant
    else if (node.hasNext) {
      val nextNode = node.next
      if (nextNode.value.objs(1) > sol.objs(1)) clean(sol, nextNode, 0)
      else 0
    }
    else 0
  }
  
  private def checkRight(sol: MOSol[Sol], node: sols.RBNode): Int = {      
    // Dominated Quadrant
    if (node.value.objs(1) > sol.objs(1)) clean(sol, node, 0)
    // Dominating Quadrant
    else if (node.hasPrev) {
      val prevNode = node.prev
      if (prevNode.value.objs(1) > sol.objs(1)) clean(sol, prevNode, 0)
      else 0
    }
    else 0
  }
    
  def size = sols.size
  def isEmpty = sols.isEmpty
  def map[B](f: (MOSol[Sol]) => B): IndexedSeq[B] = sols.toList.map(n => f(n)).toIndexedSeq
  def mkString(s: String) = sols.toList.mkString(s)
  def foreach[B](f: (MOSol[Sol]) => B) = sols.toList.foreach(n => f(n))
  def removeAll() { sols.removeAll }
  def filter(f: (MOSol[Sol]) => Boolean) = sols.toList.filter(f)
  def toArray: Array[MOSol[Sol]] = sols.toList.toArray
  def sortBy(f: (MOSol[Sol]) => Int) = sols.toList.sortBy(f)
  def max(f: (MOSol[Sol]) => Int): MOSol[Sol] = min(-f(_))
  
  def sortedByObj(obj: Int) = {
    if (obj == 0) sols.toList 
    else if (obj == 1) sols.toReversedList
    else throw new IllegalArgumentException("biobjective pareto set")  
  }

  def min(f: (MOSol[Sol]) => Int): MOSol[Sol] = {
    val list = sols.toList
    var minValue = Int.MaxValue
    var min: MOSol[Sol] = null
    for (x <- list) {
      val value = f(x)
      if (value < minValue) {
        minValue = value
        min = x
      }
    }
    min
  }

  override def toString: String =  sols.toList.toString
}

object RBPareto {
  def apply[Sol]() = new RBPareto[Sol]
}