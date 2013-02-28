package oscar.cp.mem.dominance

/**
 * DomQuadTree
 *
 *  @author: Renaud Hartert - ren.hartert@gmail.com
 */

class HabenichtQuadTree[V](private val nDim: Int) {

  private val nQuadrants = 1 << nDim
  private val Quadrants = 0 until nQuadrants
  private val usedQuadrants = 1 until nQuadrants - 1

  private val bestQuad = Quadrants.min
  private val worstQuad = Quadrants.max

  private var root: QuadNode = Empty

  // Data structure
  // --------------

  abstract class QuadNode {
    def isEmpty: Boolean
    def child(quad: Int): QuadNode
    def hasChild(quad: Int): Boolean
    def insertQuad(k: Array[Int]): Int
  }

  case object Empty extends QuadNode {
    def isEmpty = true
    def child(quad: Int): QuadNode = throw new NoSuchElementException("Empty node")
    def hasChild(quad: Int): Boolean = false
    def insertQuad(k: Array[Int]): Int = throw new NoSuchElementException("Empty node")
  }

  case class NonEmpty(keys: Array[Int], value: V) extends QuadNode {

    private[HabenichtQuadTree] val children: Array[QuadNode] = Array.fill(nQuadrants - 2)(Empty)
    private[HabenichtQuadTree] var father: QuadNode = Empty
    private[HabenichtQuadTree] var quad: Int = bestQuad

    def isEmpty = false
    def child(quad: Int) = children(quad)
    def hasChild(quad: Int) = !children(quad).isEmpty

    // Returns the id of the relevant quadrant of the QuadTree (pareto dominance based)
    def insertQuad(k: Array[Int]): Int = insertQuad0(k, 0, 0, false, false)
    private def insertQuad0(k: Array[Int], dim: Int, quadrant: Int, dom: Boolean, notDom: Boolean): Int = {
      if (dim == nDim) if (dom && !notDom) bestQuad else quadrant
      else if (k(dim) < keys(dim)) insertQuad0(k, dim + 1, quadrant << 1, true, notDom)
      else if (k(dim) == keys(dim)) insertQuad0(k, dim + 1, (quadrant << 1) + 1, dom, notDom)
      else insertQuad0(k, dim + 1, (quadrant << 1) + 1, dom, true)
    }

    override def toString: String = "NonEmpty(keys: " + keys.mkString(",") + ", value: " + value + ")"
  }

  // Insertion
  // ---------

  def insert(keys: Array[Int], value: V) {
    val newNode = NonEmpty(keys, value)
    if (root.isEmpty) root = newNode
    else process(newNode, toNonEmpty(root)) // Safe cast
  }
  
  // Process
  // -------

  def process(newNode: NonEmpty, root: NonEmpty) {
    
    val quad = root.insertQuad(newNode.keys)
    
    if (quad == bestQuad) replace(newNode, root)
    else if (quad != worstQuad){     
      // Test 2
      for (q <- asGood(quad) if q != quad) {
        if (root.hasChild(q)) {
          val child = toNonEmpty(root.child(q)) // Safe cast
          if (checkDominance(newNode, child)) return
        }
      }    
      // Test 1
      for (q <- asBad(quad) if q != quad) {
        if (root.hasChild(q)) {
          val child = toNonEmpty(root.child(q)) // Safe cast
          root.children(q) = filterDominance(newNode, child)
        }
      }
      // Keep processing
      if (root.hasChild(quad)) process(newNode, toNonEmpty(root.child(quad))) // Safe cast
      // Insertion
      else {
        root.children(quad) = newNode
        newNode.quad = quad
        newNode.father = root
      }
    }
  }

  // Test 2
  // ------

  def checkDominance(newNode: NonEmpty, node: QuadNode): Boolean = {
    if (node.isEmpty) false
    else {
      val quad = node.insertQuad(newNode.keys)
      if (quad == worstQuad) true
      else checkDominance(asGood(quad), newNode, node)
    }
  }

  def checkDominance(quads: IndexedSeq[Int], newNode: NonEmpty, node: QuadNode): Boolean = {
    if (quads.isEmpty) false
    else if (checkDominance(newNode, node.child(quads.head))) true
    else checkDominance(quads.tail, newNode, node)
  }

  // Test 1
  // ------

  def filterDominance(newNode: NonEmpty, root: NonEmpty): QuadNode = {

    val quad = newNode.insertQuad(root.keys)
    
    if (quad == worstQuad) {
      val newRoot = delete(root) 
      if (!newRoot.isEmpty) filterDominance(newNode, toNonEmpty(newRoot)) // Safe cast
      else Empty      
    }
    else {
      for (q <- asBad(quad)) {
        if (root.hasChild(q)) {
          val child = toNonEmpty(root.child(q)) // Safe cast
          filterDominance(newNode, child)
        }
      }
      root
    }
  }
  
  // Replace
  // -------
  
  def replace(newNode: NonEmpty, oldRoot: NonEmpty) {
    
    move(newNode, oldRoot)
    for (q <- usedQuadrants) {
      if (oldRoot.hasChild(q)) {
        val child = toNonEmpty(oldRoot.child(q)) // Safe cast
        reconsider(child, newNode)
      }
    }
  }

  // Reconsider
  // ----------

  private def reconsider(node: NonEmpty, newRoot: NonEmpty) {  
    // Insert children first
    for (q <- usedQuadrants) {
      val child = toNonEmpty(node.child(q)) // Safe cast
      reconsider(child, newRoot)
    }
    // Insertion
    val quad = newRoot.insertQuad(node.keys)
    if (quad != worstQuad) insert(node, newRoot)
  }
  
  // Reinsert 
  // -----------

  private def reinsert(node: QuadNode, newRoot: NonEmpty) {
    if (!node.isEmpty) {
      val nonEmpty = toNonEmpty(node) // Safe cast    
      // Insert children first
      for (q <- usedQuadrants) {
        reinsert(node.child(q), newRoot)
      }
      // Insertion
      insert(newRoot, nonEmpty)
    }
  }

  private def insert(node: NonEmpty, newRoot: NonEmpty) {
    val quad = newRoot.insertQuad(node.keys)
    // Keep searching
    if (newRoot.hasChild(quad)) insert(node, toNonEmpty(newRoot.child(quad))) // Safe cast
    // Insert
    else {
      newRoot.children(quad) = node
      node.father = newRoot
      node.quad = quad
    } 
  }

  // Delete 
  // ------
  
  private def delete(oldRoot: NonEmpty): QuadNode =  {
    val first = firstChild(usedQuadrants.min, oldRoot)
    if (first == worstQuad) Empty // No children
    else {
      val newRoot = toNonEmpty(oldRoot.child(first)) // Safe cast
      move(newRoot, oldRoot)
      for (q <- first+1 until worstQuad) {
        reinsert(oldRoot.child(q), newRoot)
      }
      newRoot
    }
  }

  private def firstChild(quad: Int, node: NonEmpty): Int = {
    if (quad == worstQuad) worstQuad
    else if (node.hasChild(quad)) quad
    else firstChild(quad + 1, node)
  }
    
  def deleteNode(node: NonEmpty) {
    if (node.father.isEmpty) root = Empty
    else {
      val father = toNonEmpty(node.father) // Safe cast
      father.children(node.quad) = Empty
    }
  }
  
  def move(newNode: NonEmpty, oldNode: NonEmpty) {
    if (oldNode.father.isEmpty) root = newNode
    else {
      val father = toNonEmpty(oldNode.father) // Safe cast
      father.children(oldNode.quad) = newNode
      newNode.father = father
      newNode.quad = oldNode.quad
    }
  }

  // Utils 
  // -----

  // Returns the opposite quadrant
  private def opposite(quadId: Int): Int = quadId ^ worstQuad

  // Returns the list of quadrants that could contain best solutions
  private def asGood(quad: Int): IndexedSeq[Int] = {
    (quad until nQuadrants).filter(q => (q - (q ^ quad)) == quad)
  }

  // Returns the list of quadrants that could contain worst solutions
  private def asBad(quad: Int): IndexedSeq[Int] = {
    val dim = opposite(quad)
    (0 to quad).filter(q => ((q ^ dim) - q) == dim)
  }

  // Returns the lists of the sub-quadrants of quad that are:
  // list 1 : non-adjacent to the worst quadrant
  // list 2 : adjacent to the worst quadrant
  private def adjacents(quad: Int): (IndexedSeq[Int], IndexedSeq[Int]) = {
    val sameDims = opposite(quad)
    Quadrants.partition(q => (q - (q ^ sameDims)) != sameDims)
  }
  // Shortcut for pattern matching
  private def toNonEmpty(tree: QuadNode): NonEmpty = tree.asInstanceOf[NonEmpty]
}
