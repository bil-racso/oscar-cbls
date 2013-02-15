package oscar.cp.mem.pareto

class RBPareto[Sol] extends Pareto[Sol] {

  val nObjs = 2
  
  val nadir: Array[Int] = Array.fill(nObjs)(Int.MaxValue)
  val ideal: Array[Int] = Array.fill(nObjs)(0)
  
  private val sols: RBLinkedTree[MOSol[Sol]] = new RBLinkedTree()
  
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
      if (node.hasNext) {
        val next = node.next
        removeSol(node)
        clean(sol, next, n+1)
      }
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

    // Empty front
    if (sols.isEmpty) {      
      updateBounds(sols.insert(sol.objs(0), sol))
      0
    } 
    // Check dominance
    else {          
      val closestNode = sols.find(sol.objs(0))
      val n = checkNode(sol, closestNode)
      if (n == 0) updateBounds(sols.insertIn(sol.objs(0), sol, closestNode))
      else if (n > 0) updateBounds(sols.insert(sol.objs(0), sol)) // may have removed closest node
      n
    }
  }
  
  def getDominant(sol: Array[Int]): Option[MOSol[Sol]] = {   
    if (sols.isEmpty) None
    else {      
      // Get the lower closest solution (if any)
      var closestNode = sols.find(sol(0))
      if (closestNode.value.objs(0) > sol(0)) {
        if (closestNode.hasPrev) closestNode = closestNode.prev
        else None
      }
      // Returns the solution if it dominates sol
      if (closestNode.value.objs(1) <= sol(1)) Some(closestNode.value)
      else None
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
      if (nextNode.value.objs(1) > sol.objs(1)) {
        clean(sol, nextNode, 0)
      }
      else 0
    }
    else 0
  }
  
  private def checkRight(sol: MOSol[Sol], node: sols.RBNode): Int = {      
    // Dominated Quadrant
    if (node.value.objs(1) > sol.objs(1)) {
      clean(sol, node, 0)
    }
    // Dominating Quadrant
    else if (node.hasPrev) {
      val prevNode = node.prev
      if (prevNode.value.objs(1) <= sol.objs(1)) -1
      else 0
    }
    else 0
  }
    
  def size = sols.size
  def map[B](f: (MOSol[Sol]) => B): List[B] = sols.toList.map(n => f(n))
  def mkString(s: String) = sols.toList.mkString(s)
  def foreach[B](f: (MOSol[Sol]) => B) = sols.toList.foreach(n => f(n))
  def removeAll() { sols.removeAll }
  def filter(f: (MOSol[Sol]) => Boolean) = sols.toList.filter(f)
  def toList: List[MOSol[Sol]] = sols.toList
  def sortBy(f: (MOSol[Sol]) => Int) = sols.toList.sortBy(f)
  
  def sortByObj(obj: Int) = {
    if (obj == 0) sols.toList 
    else if (obj == 1) sols.toReversedList
    else throw new IllegalArgumentException("biobjective pareto set")  
  }

  def min(f: (MOSol[Sol]) => Int): MOSol[Sol] = {
    var node = sols.first
    var minValue = f(node.value)
    var min = node.value
    while(node.hasNext) {
      node = node.next
      val value = f(node.value)
      if (value < minValue) {
        minValue = value
        min = node.value
      }
    }
    min
  }

  override def toString: String =  sols.toList.toString
}

object RBPareto {
  def apply[Sol]() = new RBPareto[Sol]
}