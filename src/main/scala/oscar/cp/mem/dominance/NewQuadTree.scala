package oscar.cp.mem.dominance

class NewQuadTree[V](private val nDim: Int, max: Boolean = false) {

  private val nQuadrants = 1 << nDim
  private val Quadrants = 0 until nQuadrants
  private val UsedQuadrants = 1 until nQuadrants - 1

  private val bestQuad = Quadrants.max
  private val worstQuad = Quadrants.min

  private var root: Option[Node] = None

  class Node(val keys: Array[Int], val value: V) {
    val children: Array[Option[Node]] = Array.fill(nQuadrants)(None)
    var quadrant: Int = 0
    var father: Option[Node] = None
    def hasChild(q: Int) = children(q).isDefined
    def hasFather = father.isDefined
    override def toString = "Node("+keys.mkString(", ")+", "+value+")"
  }

  def insert(keys: Array[Int], value: V): Boolean = {
    if (root.isDefined) process(root.get, new Node(keys, value))
    else {
      root = Some(new Node(keys, value))
      true
    }
  }

  def process(R: Node, cand: Node): Boolean = {

    val quad = position(R, cand)

    if (quad == worstQuad) return false
    else if (quad == bestQuad) {
      replace(R, cand)
      return true
    }
    else {

      // Check
      for (q <- betterQuads(quad) if R hasChild q) {
        if (isDominated(R.children(q).get, cand)) return false
      }

      // Removes
      for (q <- worseQuads(quad) if R hasChild q) {
        removeDominated(R.children(q).get, cand, 0)
      }

      // Insert
      if (R hasChild quad) process(R.children(quad).get, cand) // Safe
      else {
        R.children(quad) = Some(cand)
        cand.quadrant = quad
        cand.father = Some(R)
        return true
      }
    }
  }

  def isDominated(R: Node, cand: Node): Boolean = {
    val quad = position(R, cand)
    if (quad == worstQuad) return true
    else {
      for (q <- asGoodQuads(quad) if R hasChild q) {
        if (isDominated(R.children(q).get, cand)) return true
      }
      return false
    }
  }

  def removeDominated(R: Node, cand: Node, bd: Int) {
    val quad = position(R, cand)
    if (quad == bestQuad) deleteAndRepair(R, cand, quad, bd)
    else {
      for (q <- asBadQuads(quad) if R hasChild q) {
        removeDominated(R.children(q).get, cand, computeBoundedVector(quad, q, bd))
      }
    }
  }

  def deleteAndRepair(R: Node, cand: Node, fQuad: Int, bd: Int) {
    val (remainingQuads, first) = firstChild(R, betterQuads(bd))
    if (first == Quadrants.max) detach(R)
    else {     
      val newRoot = R.children(first).get
      transplant(R, newRoot)     
      for (q <- remainingQuads if R hasChild q) {
        //reinsertIn(R.children(q).get, newRoot)
        reinsertFilter(R.children(q).get, cand, newRoot, computeBoundedVector(fQuad, q, bd))
      }
      removeDominated(newRoot, cand, bd)
    }
  }
  
  def reinsertFilter(R: Node, domNode: Node, newRoot: Node, bd: Int) {
    val quad = position(domNode, R)   
    if (quad == worstQuad) {
      for (q <- betterQuads(bd) if R hasChild q) {
        reinsertFilter(R.children(q).get, domNode, newRoot, computeBoundedVector(quad, q, bd))
      }
    }
    else {
      for (q <- asGoodQuads(opposite(quad)) if R hasChild q) {
        reinsertFilter(R.children(q).get, domNode, newRoot, computeBoundedVector(quad, q, bd))
      }
      insert(R, newRoot)
    }
  }
  
  def firstChild(R: Node, quads: IndexedSeq[Int]): (IndexedSeq[Int], Int) = {
    if (quads.isEmpty) (IndexedSeq[Int](), Quadrants.max)
    else if (R hasChild quads.head) (quads.tail, quads.head)
    else firstChild(R, quads.tail)
  }

  def replace(R: Node, cand: Node) {
    transplant(R, cand)
    for (q <- UsedQuadrants if R hasChild q) {
      reconsiderIn(R.children(q).get, cand)
    }
  }

  def transplant(R: Node, cand: Node) {
    if (!R.hasFather) root = Some(cand)
    else {
      val father = R.father.get
      cand.father = Some(father)
      cand.quadrant = R.quadrant
      father.children(cand.quadrant) = Some(cand)
    }
  }
  
  def reconsiderIn(oldR: Node, newR: Node) {
    detach(oldR)
    val quad = position(oldR, newR)
    for (q <- UsedQuadrants if oldR hasChild q) {
      reconsiderIn(oldR.children(q).get, newR)
    }   
    if (quad != bestQuad) insert(newR, oldR)
  }

  def reinsertIn(oldR: Node, newR: Node) {
    detach(oldR)
    for (q <- UsedQuadrants if oldR hasChild q) {
      reinsertIn(oldR.children(q).get, newR)
    }    
    insert(newR, oldR)
  }

  def insert(R: Node, cand: Node) {
    val quad = position(R, cand)
    if (R hasChild quad) insert(R.children(quad).get, cand)
    else {
      R.children(quad) = Some(cand)
      cand.quadrant = quad
      cand.father = Some(R)
    }
  }

  def detach(R: Node) {
    if (R.hasFather) {
      R.father.get.children(R.quadrant) = None
      R.father = None
    }
  }

  def comp(a: Int, b: Int) = if (max) a > b else a < b

  // Returns the id of the quadrant of R in which c is positioned.
  def position(root: Node, cand: Node): Int = position0(root.keys, cand.keys, 0, 0, false, false)
  private def position0(rKeys: Array[Int], cKeys: Array[Int], d: Int, q: Int, dom: Boolean, notDom: Boolean): Int = {
    if (d == nDim) if (dom && !notDom) bestQuad else q
    else if (comp(cKeys(d), rKeys(d))) position0(rKeys, cKeys, d + 1, (q << 1) + 1, true, notDom)
    else if (cKeys(d) == rKeys(d)) position0(rKeys, cKeys, d + 1, q << 1, dom, notDom)
    else position0(rKeys, cKeys, d + 1, q << 1, dom, true)
  }

  // Returns the opposite quadrant
  private def opposite(quadId: Int): Int = quadId ^ Quadrants.max

  // Returns the list of quadrants that could contain best solutions
  def asGoodQuads(quad: Int): IndexedSeq[Int] = {
    (quad to UsedQuadrants.max).filter(q => (q - (q ^ quad)) == quad)
  }

  def betterQuads(quad: Int): IndexedSeq[Int] = {
    (quad + 1 to UsedQuadrants.max).filter(q => (q - (q ^ quad)) == quad)
  }

  // Returns the list of quadrants that could contain worst solutions
  def asBadQuads(quad: Int): IndexedSeq[Int] = {
    val dim = opposite(quad)
    (UsedQuadrants.min to quad).filter(q => ((q ^ dim) - q) == dim)
  }

  def worseQuads(quad: Int): IndexedSeq[Int] = {
    val dim = opposite(quad)
    (UsedQuadrants.min until quad).filter(q => ((q ^ dim) - q) == dim)
  }
  
  def toList: List[V] = {
    if (!root.isDefined) List()
    else toList0(root.get)
  }
  
  def toList0(node: Node): List[V] = {
    var list = List(node.value)
    for (q <- UsedQuadrants if node hasChild q) {
      list = list ::: toList0(node.children(q).get)
    }
    list
  }
  
  def computeBoundedVector(fQuad: Int, cQuad: Int, bVect: Int): Int = {
    bVect | opposite(fQuad) & opposite(cQuad)
  }
  
  override def toString = "{"+toList.mkString(", ")+"}"
}

object NewQuadTree {
  def apply[V](nObjs: Int, max: Boolean = false) = new NewQuadTree[V](nObjs, max)
}