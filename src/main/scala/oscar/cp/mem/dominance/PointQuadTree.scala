package oscar.cp.mem.dominance

class PointQuadTree[V](private val nDim: Int) {

  private val Dim = 0 until nDim
  private val nQuadrants = 1 << nDim
  private val Quadrants = 0 until nQuadrants
  
  private val quadrants = Array.tabulate(nQuadrants)(q => computeBitQuadrant(q))
  
  private def computeBitQuadrant(quad: Int): Array[Boolean] = {
    var q = quad
    Array.tabulate(nDim)(d => {
      val b = q%2
      q = q/2
      b == 0
    })
  }

  case class QuadTree(keys: Array[Int], value: V) {

    val children: Array[Option[QuadTree]] = Array.fill(nQuadrants)(None)

    def insert(k: Array[Int], v: V): QuadTree = {
      val quad = getQuadrant(k)
      if (children(quad).isDefined) children(quad).get.insert(k, v)
      else children(quad) = Some(QuadTree(k, v))
      this
    }
    
    def getAdjacentQuadrants(quad: Int): List[(Int, Int)] = {
      getAdjacentQuadrants(quadrants(quad), 0)
    }
    
    def getAdjacentQuadrants(bitQuad: Array[Boolean], dim: Int): List[(Int, Int)] = {
      if (dim == nDim) Nil
      else {
        val newBitQuad = bitQuad
        newBitQuad(dim) = !bitQuad(dim)
        (dim, computeQuadrantId(newBitQuad, 0, 0)) :: getAdjacentQuadrants(bitQuad, dim+1)
      }
    }
    
    def computeQuadrantId(bitQuad: Array[Boolean], id: Int, dim: Int): Int = {
      if (dim == nDim) id
      else if (bitQuad(dim)) computeQuadrantId(bitQuad, (id << 1) + 1, dim+1)
      else computeQuadrantId(bitQuad, (id << 1), dim+1)
    }
    
    // Samet remove algorithm
    def remove(): QuadTree = {
      
      // Check children !!!
            
      // Select a root candidate     
      val (refQuad, candidate) = selectCandidate
      val oppQuad = oppQuadrant(refQuad)
      
      // Compute the new root
      val nRoot = QuadTree(candidate.keys, candidate.value)
      for (q <- Quadrants) nRoot.children(q) = children(q)
      
      // Compute all the adjacent quadrants with dimensions
      val adjacents = getAdjacentQuadrants(oppQuad)
      
      // Apply on each adjacentQuads
      for ((dim, adjQuad) <- adjacents) {
        if (nRoot.children(adjQuad).isDefined) {
          val child = nRoot.children(adjQuad).get
          child.adjQuadrant(dim, adjQuad, oppQuad, nRoot, nRoot)
        }
      }
          
      // Apply to the first node in refQuad and call recursively until reaching candidate
      val child = nRoot.children(refQuad).get
      child.newRoot(refQuad, this, nRoot, candidate)
          
      // The new me (needs to replace A and B)
      nRoot
    }
    
    def adjQuadrant(dim: Int, outQuad: Int, inQuad: Int, father: QuadTree, nRoot: QuadTree) {    
            
      // Reinsertion
      if (nRoot.getQuadrant(this) == inQuad) nRoot.reinsert(this, outQuad, father)
      // No reinsertion
      else {
        // Get the remaining sub-quadrants adjacent to inQuad
        val remainingQuad = Quadrants.filter(quadrants(_)(dim) == quadrants(inQuad)(dim))
        // Process separately each remaining quadrant
        for (q <- remainingQuad) {
          if (this.children(q).isDefined) {
            val child = this.children(q).get
            child.adjQuadrant(dim, q, inQuad, this, nRoot)
          }
        }
      }
    }
     
    def newRoot(outQuad: Int, father: QuadTree, nRoot: QuadTree, cand: QuadTree) {
      
      val adjacents = getAdjacentQuadrants(outQuad)
      
      for((dim, adjQuad) <- adjacents) {
        if (this.children(adjQuad).isDefined) {
          val child = this.children(adjQuad).get
          child.adjQuadrant(dim, outQuad, adjQuad, this, nRoot)
        }
      }      
      
      if (this == cand) father.children(oppQuadrant(outQuad)) = None
      else {
        val child = children(oppQuadrant(outQuad)).get
        child.newRoot(outQuad, this, nRoot, cand)
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
    
    def getCandidates: List[(Int, QuadTree)] = getCandidates0(0)
    
    def getCandidates0(quad: Int): List[(Int, QuadTree)] = {
      if (quad == nQuadrants) Nil 
      else if (children(quad).isDefined) (quad, children(quad).get)::getCandidates0(quad)
      else getCandidates0(quad)
    }
    
    // TODO !!
    // There is at most 2 candidates
    def selectMinAxis(candidates: List[(Int, QuadTree)]): List[(Int, QuadTree)] = {
      candidates
    }
    
    // Selects the candidate with the lower Manhattan distance
    def minManhattan(candidates: List[(Int, QuadTree)]): (Int, QuadTree) = candidates match {
      case Nil => throw new NoSuchElementException("no candidates")
      case h::Nil => h
      case (quad, cand)::tail => minManhattan(tail, (quad, cand), computeManhattan(cand))
    }
    
    def minManhattan(candidates: List[(Int, QuadTree)], min: (Int, QuadTree), minVal: Int): (Int, QuadTree) = candidates match {
      case Nil => min
      case (quad, cand)::tail => {
        val m = computeManhattan(cand)
        if (m < minVal) minManhattan(tail, (quad, cand), m)
        else minManhattan(tail, min, minVal)
      }
    }
    
    // Compute the Manhattan distance 
    def computeManhattan(cand: QuadTree): Int = {
      var dist = 0
      for (d <- Dim) dist += math.abs(cand.keys(d) - keys(d))
      dist
    }
    
    def closest(cand1: QuadTree, cand2: QuadTree, dim: Int): Boolean = {
      true
    }

    // Returns the id of the corresponding quadrant
    def getQuadrant(quadTree: QuadTree): Int = getQuadrant(quadTree.keys, 0, 0)
    def getQuadrant(k: Array[Int]): Int = getQuadrant(k, 0, 0)

    private def getQuadrant(k: Array[Int], dim: Int, quadrant: Int): Int = {
      if (dim == k.size) quadrant
      else if (k(dim) <= keys(dim)) getQuadrant(k, dim + 1, (quadrant << 1) + 1)
      else getQuadrant(k, dim + 1, quadrant << 1)
    }

    // Returns the id of the opposite quadrant
    def oppQuadrant(quadrant: Int) = nQuadrants - (quadrant + 1)
    
    override def toString = "QuadTree(keys: ("+keys.mkString(",")+"), value: "+value+")"
  }
}

object QuadTree {

  def apply[V](nDim: Int) = {
    new PointQuadTree[V](nDim)
  }

  def main(args: Array[String]) {

    val p = QuadTree[String](2)

    val tree = p.QuadTree(Array(35, 42), "Chicago")
    tree insert (Array(52, 10), "Mobile")
    tree insert (Array(62, 77), "Toronto")
    tree insert (Array(82, 65), "Buffalo")
    tree insert (Array(27, 35), "Omaha")
    tree insert (Array(5, 45), "Denver")
    
    //tree.remove()

    println(tree)
  }
}