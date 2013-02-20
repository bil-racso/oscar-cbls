package oscar.cp.mem.dominance

class DominatedPointQuadTree[V](private val nDim: Int) {
  
  private val nQuadrants = 1 << nDim
  private val Quadrants = 0 until nQuadrants
  private val usedQuadrants = 1 until nQuadrants-1
  
  // Better solutions
  private val bestQuad = Quadrants.min
  // Worse solutions
  private val worstQuad = Quadrants.max
  
  // Quadrants utils 
  // ---------------

  private def opposite(quadId: Int): Int = quadId ^ worstQuad
  
  // This function is the tricky MO part of the structure!
  // Regarding a quadrant in the removing phase, this function returns the set 
  // of "safe" quadrants and the set of "not-safe" quadrants.
  private def safeQuadrants(diff: Int): (IndexedSeq[Int], IndexedSeq[Int]) = {
    val sameDims = opposite(diff)
    Quadrants.partition(q => (q - (q ^ sameDims)) != sameDims)
  }
  
  // The QuadTree
  // ------------
  
  abstract class QuadTree {
    def isEmpty: Boolean
    def child(quad: Int): QuadTree
    def hasChild(quad: Int): Boolean
    def insert(keys: Array[Int], value: V): QuadTree 
  }
  
  case object Empty extends QuadTree {
    def isEmpty = true
    def child(quad: Int): QuadTree = throw new NoSuchElementException("Empty QuadTree")   
    def hasChild(quad: Int): Boolean = false
    def insert(keys: Array[Int], value: V): QuadTree = NonEmpty(keys, value)
  }
  
  case class NonEmpty(keys: Array[Int], value: V) extends QuadTree {
    
    private val children: Array[QuadTree] = Array.fill(nQuadrants)(Empty)
    
    def isEmpty = false
    def child(quad: Int) = children(quad)
    def hasChild(quad: Int) = !children(quad).isEmpty
    
    def insert(keys: Array[Int], value: V): QuadTree = insert0(NonEmpty(keys, value), Empty, 0)
    
    private def insert0(tree: NonEmpty, father: QuadTree, currentQuad: Int): QuadTree = {
      
      val quad = getQuadrant(tree.keys)
      
      // The new solution is dominated by this node
      if (quad == worstQuad) this      
      
      // The new solution dominates this node
      else if (quad == bestQuad) dominated(tree, father, currentQuad)  
      
      // The new solution is Pareto equivalent to this node
      else {
        if (!hasChild(quad)) children(quad) = tree 
        else {
          val child = toNonEmpty(children(quad)) // Safe
          child.insert0(tree, this, quad)
        }
        this
      }
    }
    
    private def dominated(newRoot: NonEmpty, father: QuadTree, quad: Int): QuadTree = {
      
      // This node is replaced by newRoot
      switch(father, quad, newRoot)
      
      // Move the sub-QuadTrees to newRoot
      for (q <- usedQuadrants) newRoot.children(q) = children(q)
      
      // Remove the remaining dominated solutions
      for (q <- usedQuadrants if hasChild(q)) {
        val child = toNonEmpty(children(q)) // Safe
        child.removeDominated(q, newRoot, newRoot)
      }
      
      newRoot
    }
    
    private def removeDominated(quad: Int, father: QuadTree, newRoot: NonEmpty) {
      
      // List of non-adjacent and adjacent quadrants to the worst quadrant
      val (safeQuads, notSafeQuads) = safeQuadrants(quad)
      
      // This node is dominated by newRoot
      if (newRoot.getQuadrant(keys) == worstQuad) {
        // Remove this node node 
        prune(father, quad)
        // Reinsert potentially non-dominated children
        for (q <- safeQuads if hasChild(q)) {        
          val child = toNonEmpty(children(q)) // Safe
          child.reinsert(quad, father, newRoot)
        }
      }     
      
      // This node is not dominated by newRoot
      else {
        // Search for potentially dominated children
        for (q <- notSafeQuads if hasChild(q)) {
          val child = toNonEmpty(children(q)) // Safe
          child.removeDominated(q, this, newRoot)
        }
      }      
    }
    
    private def reinsert(quad: Int, father: QuadTree, newRoot: NonEmpty) {
            
      // Reinsert the node (does not dominate newRoot)
      prune(father, quad)
      newRoot.insert0(this, Empty, 0)
      
      // Reinsert the children (does not dominate newRoot)
      for(q <- usedQuadrants if hasChild(q)) {
        val child = toNonEmpty(children(q)) // Safe
        child.reinsert(q, this, newRoot)
      }     
    }    
    
    // Remove the child in quadrant quad of father
    private def prune(father: QuadTree, quad: Int) = switch(father, quad, Empty)
    
    // Change the child in quadrant quad of father into tree 
    private def switch(father: QuadTree, quad: Int, tree: QuadTree) = if (!father.isEmpty) {
      toNonEmpty(father).children(quad) = tree // Safe
    }
    
    // Returns the id of the relevant quadrant of the QuadTree (pareto dominance based)
    private def getQuadrant(k: Array[Int]): Int = getQuadrant0(k, 0, 0, false, false)
    private def getQuadrant0(k: Array[Int], dim: Int, quadrant: Int, dom: Boolean, notDom: Boolean): Int = {
      if (dim == k.size) if (dom && !notDom) bestQuad else quadrant
      else if (k(dim) < keys(dim)) getQuadrant0(k, dim + 1, quadrant << 1, true, notDom)
      else if (k(dim) == keys(dim)) getQuadrant0(k, dim + 1, (quadrant << 1) + 1, dom, notDom)
      else getQuadrant0(k, dim + 1, (quadrant << 1) + 1, dom, true)
    }
    
    // Shortcut for pattern matching
    private def toNonEmpty(tree: QuadTree): NonEmpty = tree.asInstanceOf[NonEmpty]
  }
}

object DominatedPointQuadTree {
  
  def apply[V](nDim: Int): DominatedPointQuadTree[V]#QuadTree = new DominatedPointQuadTree[V](nDim).Empty

  def main(args: Array[String]) {

    var tree = DominatedPointQuadTree[String](3)

    tree = tree.insert(Array(5, 5, 5), "A")
    tree = tree.insert(Array(3, 4, 7), "C")
    tree = tree.insert(Array(5, 8, 2), "D")
    tree = tree.insert(Array(7, 4, 3), "E")
    tree = tree.insert(Array(8, 2, 5), "F")

    tree = tree.insert(Array(3, 3, 3), "B")
  }
  
}

