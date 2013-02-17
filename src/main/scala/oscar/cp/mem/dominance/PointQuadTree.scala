package oscar.cp.mem.dominance

class PointQuadTree[V](private val nDim: Int) {

  private val Dim = 0 until nDim
  private val nQuadrants = 1 << nDim
  private val Quadrants = 0 until nQuadrants
  
  def adjacent(quadId: Int, dim: Int) = quadId ^ (1 << dim)
  
  def opposite(quadId: Int): Int = quadId ^ ((1 << nDim) - 1)
  
  def adjacents(quadId: Int): List[(Int, Int)] = adjacents0(quadId, 0)
    
  private def adjacents0(quadId: Int, dim: Int): List[(Int, Int)] = {
   if (dim == nDim) Nil
   else (dim, adjacent(quadId, dim)) :: adjacents0(quadId, dim+1)
  }
  
  def sameDimension(quadId: Int, dim: Int): IndexedSeq[Int] = {
    val access = (1 << dim)
    val diff = quadId - (quadId ^ access)
    Quadrants.filter(q => (q - (q ^ access)) == diff)
  }
  
  abstract class Tree {
    def isEmpty: Boolean
    def insert(k: Array[Int], v: V): Tree
    def remove(): Tree
  }
  
  case object Empty extends Tree {
    def isEmpty = true
    def insert(k: Array[Int], v: V) = QuadTree(k, v)
    def remove() = this
  }
  
  case class QuadTree(keys: Array[Int], value: V) extends Tree {
    
    def isEmpty = false

    // One children in each quadrant
    private val children: Array[Option[QuadTree]] = Array.fill(nQuadrants)(None)
    
    def hasChild(quadId: Int): Boolean = children(quadId).isDefined

    def insert(k: Array[Int], v: V): Tree = {
      val quad = getQuadrant(k)
      if (children(quad).isDefined) children(quad).get.insert(k, v)
      else children(quad) = Some(QuadTree(k, v))
      this
    }

    // Samet remove algorithm
    def remove(): Tree = {
      if (!children.exists(_.isDefined)) Empty
      else {
        // Select a root candidate     
        val (refQuad, candidate) = selectCandidate
        // Compute the new root
        val nRoot = QuadTree(candidate.keys, candidate.value)
        for (q <- Quadrants) nRoot.children(q) = children(q)
        // Reajust the tree
        reajust(nRoot, candidate, opposite(refQuad), refQuad)
        nRoot
      }
    }
    
    def reajust(nRoot: QuadTree, candidate: QuadTree, oppQuad: Int, refQuad: Int) = {         
      // Phase 1
      for ((dim, adjQuad) <- adjacents(oppQuad)) {
        if (nRoot.hasChild(adjQuad)) {
          val child = nRoot.children(adjQuad).get
          child.adjQuadrant(dim, adjQuad, oppQuad, nRoot, nRoot)
        }
      }
      // Phase 2
      val child = nRoot.children(refQuad).get
      child.newRoot(adjacents(refQuad), oppQuad, this, nRoot, candidate)
    }

    def adjQuadrant(dim: Int, outQuad: Int, inQuad: Int, father: QuadTree, nRoot: QuadTree) {
      // Reinsertion
      if (nRoot.getQuadrant(this.keys) == inQuad) {
        nRoot.reinsert(this, outQuad, father)
      }
      // No reinsertion
      else {
        // Get the remaining sub-quadrants adjacent to inQuad
        val remainingQuad = sameDimension(inQuad, dim)
        // Process separately each remaining quadrant
        for (q <- remainingQuad) {
          if (this.hasChild(q)) {
            val child = this.children(q).get
            child.adjQuadrant(dim, q, inQuad, this, nRoot)
          }
        }
      }
    }

    def newRoot(adjQuads: List[(Int, Int)], goQuad: Int, father: QuadTree, nRoot: QuadTree, cand: QuadTree) {
      for ((dim, adjQuad) <- adjQuads) {
        if (this.children(adjQuad).isDefined) {
          val child = this.children(adjQuad).get
          child.adjQuadrant(dim, adjQuad, adjQuad, this, nRoot)
        }
      }
      if (this == cand) father.children(goQuad) = None
      else {
        val child = children(goQuad).get
        child.newRoot(adjQuads, goQuad, this, nRoot, cand)
      }
    }

    // Selects a root candidate
    def selectCandidate: (Int, QuadTree) = {
      // Select a root candidate in each quadrant
      val candidates = getCandidates
      // First selection criterion
      val candAxis = selectMinAxis(candidates)
      // Select the final candidate
      minManhattan(if (candAxis.isEmpty) candidates else candAxis)
    }

    def getCandidates: List[(Int, QuadTree)] = getCandidates0(0)

    private def getCandidates0(quad: Int): List[(Int, QuadTree)] = {
      if (quad == nQuadrants) Nil
      else if (!this.hasChild(quad)) getCandidates0(quad+1)
      else {
        val oppQuad = opposite(quad)
        val candidate = this.children(quad).get.deep(oppQuad)       
        (quad, candidate) :: getCandidates0(quad+1)
      }     
    }
    
    def deep(quad: Int): QuadTree = {
      if (!this.hasChild(quad)) this
      else this.children(quad).get.deep(quad)
    }

    // TODO !!
    // There is at most 2 candidates
    def selectMinAxis(candidates: List[(Int, QuadTree)]): List[(Int, QuadTree)] = {
      candidates
    }
        
    def closest(cand1: QuadTree, cand2: QuadTree, dim: Int): Boolean = {
      true
    }

    // Selects the candidate with the lower Manhattan distance
    def minManhattan(candidates: List[(Int, QuadTree)]): (Int, QuadTree) = candidates match {
      case Nil => throw new NoSuchElementException("no candidates")
      case h :: Nil => h
      case (quadId, cand) :: tail => minManhattan0(tail, (quadId, cand), manhattan(cand))
    }

    private def minManhattan0(candidates: List[(Int, QuadTree)], min: (Int, QuadTree), minVal: Int): (Int, QuadTree) = candidates match {
      case Nil => min
      case (quadId, cand) :: tail => {
        val m = manhattan(cand)
        if (m < minVal) minManhattan0(tail, (quadId, cand), m)
        else minManhattan0(tail, min, minVal)
      }
    }

    // Compute the Manhattan distance 
    def manhattan(cand: QuadTree): Int = manhattan0(cand, 0, 0)
    
    private def manhattan0(cand: QuadTree, dist: Int, dim: Int): Int = {
      if (dim == nDim) dist
      else manhattan0(cand, dist + math.abs(cand.keys(dim) - this.keys(dim)), dim+1)
    }
        
    def reinsert(child: QuadTree, outQuad: Int, father: QuadTree) {
      father.children(outQuad) = None
      insert(child.keys, child.value)
      // Reinsert each defined child
      for (q <- Quadrants if child.children(q).isDefined) {
        reinsert(child.children(q).get, q, child)
      }
    }

    def insertQuadTree(quadTree: QuadTree) {
      val quad = getQuadrant(quadTree.keys)
      if (children(quad).isDefined) children(quad).get.insertQuadTree(quadTree)
      else children(quad) = Some(quadTree)
    }

    // Returns the id of the corresponding quadrant
    def getQuadrant(k: Array[Int]): Int = getQuadrant0(k, 0, 0)

    private def getQuadrant0(k: Array[Int], dim: Int, quadrant: Int): Int = {
      if (dim == k.size) quadrant
      else if (k(dim) <= keys(dim)) getQuadrant0(k, dim + 1, (quadrant << 1) + 1)
      else getQuadrant0(k, dim + 1, quadrant << 1)
    }

    override def toString = "QuadTree(keys: (" + keys.mkString(",") + "), value: " + value + ")"
  }
}

object QuadTree {

  def apply[V](nDim: Int) = {
    new PointQuadTree[V](nDim)
  }
  
  def getTree[V](nDim: Int) = {
    val pqt = new PointQuadTree[V](nDim)
    pqt.Empty
  }

  def main(args: Array[String]) {
    
    val vv = QuadTree.getTree[String](3)

    val p = QuadTree[String](2)

    val tree = p.QuadTree(Array(35, 42), "Chicago")
    tree insert (Array(52, 10), "Mobile")
    tree insert (Array(62, 77), "Toronto")
    tree insert (Array(82, 65), "Buffalo")
    tree insert (Array(27, 35), "Omaha")
    tree insert (Array(5, 45), "Denver")
    tree insert (Array(85, 15), "Atlanta")
    tree insert (Array(90, 5), "Miami")

    val cand = tree.getCandidates

    println(tree)
  }
}