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

  private val bestQuad = Quadrants.max
  private val worstQuad = Quadrants.min

  private var root: Option[QuadNode] = None

  // Data structure
  // --------------

  case class QuadNode(keys: Array[Int], value: V) {

    private[HabenichtQuadTree] val children: Array[Option[QuadNode]] = Array.fill(nQuadrants - 1)(None)   
    def child(quad: Int) = children(quad).get
    def hasChild(quad: Int) = !children(quad).isEmpty
    
    private[HabenichtQuadTree] var _father: Option[QuadNode] = None
    def hasFather = _father.isDefined
    def father = _father.get
    def father_= (node: QuadNode) { _father = Some(node)}
    
    private[HabenichtQuadTree] var quad: Int = bestQuad

    // Returns the id of the relevant quadrant of the QuadTree (pareto dominance based)
    def insertQuad(node: QuadNode): Int = insertQuad0(node.keys, 0, 0, false, false)
    private def insertQuad0(k: Array[Int], dim: Int, quadrant: Int, dom: Boolean, notDom: Boolean): Int = {
      if (dim == nDim) if (dom && !notDom) bestQuad else quadrant
      else if (k(dim) > keys(dim)) insertQuad0(k, dim + 1, (quadrant << 1) + 1, true, notDom)
      else if (k(dim) == keys(dim)) insertQuad0(k, dim + 1, quadrant << 1, dom, notDom)
      else insertQuad0(k, dim + 1, quadrant << 1, dom, true)
    }

    override def toString: String = "QuadNode(keys: " + keys.mkString(",") + ", value: " + value + ")"
  }

  // Insertion
  // ---------

  def insert(keys: Array[Int], value: V) {
    val newNode = QuadNode(keys, value)
    if (!root.isDefined) root = Some(newNode)
    else process(newNode, root.get) // Safe cast
  }
  
  // Process
  // -------

  def process(newNode: QuadNode, root: QuadNode) {
    
    val quad = root insertQuad newNode
    
    if (quad == worstQuad) {}
    
    else if (quad == bestQuad) replace(newNode, root)
    
    else if (!isDominated(newNode, root, better(quad))) {     
      
      // Test 1
      for (q <- worst(quad) if root hasChild q) {
        root.children(q) = filterDominance(newNode, root.child(q)) // Safe
      }
      
      // Keep processing
      if (root.hasChild(quad)) process(newNode, root.child(quad)) // Safe
      // Insertion
      else {
        root.children(quad) = Some(newNode)
        newNode.quad = quad
        newNode.father = root
      }
    }
  }

  // Test 2
  // ------

  def isDominated(newNode: QuadNode, node: QuadNode, quads: IndexedSeq[Int]): Boolean = {
    if (quads.isEmpty) false
    else if (!node.hasChild(quads.head)) isDominated(newNode, node, quads.tail)
    else if (checkDominance(newNode, node.child(quads.head))) true // Safe
    else isDominated(newNode, node, quads.tail)
  }

  def checkDominance(newNode: QuadNode, node: QuadNode): Boolean = {
    val quad = node insertQuad newNode
    if (quad == worstQuad) true
    else isDominated(newNode, node, asGood(quad))
  }

  // Test 1
  // ------

  def filterDominance(newNode: QuadNode, root: QuadNode): Option[QuadNode] = {

    val quad = newNode insertQuad root
    
    if (quad == worstQuad) {
      val newRoot = delete(root) 
      if (newRoot.isDefined) filterDominance(newNode, newRoot.get) // Safe
      else None      
    }
    else {
      for (q <- asBad(quad) if root hasChild q) {
        filterDominance(newNode, root.child(q)) // Safe
      }
      Some(root)
    }
  }
  
  // Replace
  // -------
  
  def replace(newNode: QuadNode, oldRoot: QuadNode) {   
    move(newNode, oldRoot)
    for (q <- usedQuadrants if oldRoot hasChild q) {
      reconsider(oldRoot.child(q), newNode)
    }
  }

  // Reconsider
  // ----------

  private def reconsider(node: QuadNode, newRoot: QuadNode) {     
    //Detach
    if (node.hasFather) node.father.children(node.quad) = None  
    // Insert children first
    for (q <- usedQuadrants if node hasChild q) {
      reconsider(node.child(q), newRoot) // Safe
    }
    // Insertion
    val quad = newRoot insertQuad node
    if (quad != worstQuad) insert(node, newRoot)
  }
  
  // Reinsert 
  // -----------

  private def reinsert(node: QuadNode, newRoot: QuadNode) {
    //Detach
    if (node.hasFather) node.father.children(node.quad) = None
    // Insert children first
    for (q <- usedQuadrants if node hasChild q) {
      reinsert(node.child(q), newRoot)
    }
    // Insertion
    insert(newRoot, node)
  }

  private def insert(node: QuadNode, newRoot: QuadNode) {
    val quad = newRoot insertQuad node
    // Keep searching
    if (newRoot hasChild quad) insert(node, newRoot.child(quad)) // Safe
    // Insert
    else {
      newRoot.children(quad) = Some(node)
      node.father = newRoot
      node.quad = quad
    } 
  }

  // Delete 
  // ------
  
  private def delete(oldRoot: QuadNode): Option[QuadNode] =  {
    val first = firstChild(usedQuadrants.min, oldRoot)
    if (first == Quadrants.max) None // No children
    else {
      val newRoot = oldRoot.child(first) // Safe 
      move(newRoot, oldRoot)
      for (q <- first+1 until Quadrants.max if oldRoot hasChild q) {
        reinsert(oldRoot.child(q), newRoot)
      }
      Some(newRoot)
    }
  }

  private def firstChild(quad: Int, node: QuadNode): Int = {
    if (quad == Quadrants.max) Quadrants.max
    else if (node.hasChild(quad)) quad
    else firstChild(quad + 1, node)
  }
    
  def deleteNode(node: QuadNode) {
    if (!node.hasFather) root = None
    else node.father.children(node.quad) = None // Safe
  }
  
  def move(newNode: QuadNode, oldNode: QuadNode) {
    if (!oldNode.hasFather) root = Some(newNode)
    else {
      val father = oldNode.father // Safe 
      father.children(oldNode.quad) = Some(newNode)
      newNode.father = father
      newNode.quad = oldNode.quad
    }
  }

  // Utils 
  // -----

  // Returns the opposite quadrant
  private def opposite(quadId: Int): Int = quadId ^ Quadrants.max

  // Returns the list of quadrants that could contain best solutions
  def asGood(quad: Int): IndexedSeq[Int] = {
    (quad to usedQuadrants.max).filter(q => (q - (q ^ quad)) == quad)
  }
  
  def better(quad: Int): IndexedSeq[Int] = {
    (quad+1 to usedQuadrants.max).filter(q => (q - (q ^ quad)) == quad)
  }

  // Returns the list of quadrants that could contain worst solutions
  def asBad(quad: Int): IndexedSeq[Int] = {
    val dim = opposite(quad)
    (usedQuadrants.min to quad).filter(q => ((q ^ dim) - q) == dim)
  }
  
  def worst(quad: Int): IndexedSeq[Int] = {
    val dim = opposite(quad)
    (usedQuadrants.min until quad).filter(q => ((q ^ dim) - q) == dim)
  }
}

object HabenichtQuadTree extends App {
  
  val tree = new HabenichtQuadTree[String](3)
  
  tree.insert(Array(9, 0, 4), "A")
  tree.insert(Array(6, 0, 7), "B")
  tree.insert(Array(4, 1, 8), "C")
  tree.insert(Array(4, 5, 7), "D")
  tree.insert(Array(3, 5, 8), "E")
  tree.insert(Array(5, 6, 6), "F")
  tree.insert(Array(6, 3, 5), "G")
  tree.insert(Array(6, 4, 8), "H")
  println("Finish")
}
