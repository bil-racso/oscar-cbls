package oscar.cp.mem.pareto

class RBLinkedTree[T] {
  
  abstract class TreeNode {
    private[RBLinkedTree] def isEmpty: Boolean
    
    private[RBLinkedTree] def isBlack: Boolean
    private[RBLinkedTree] def isRed = !isBlack
    private[RBLinkedTree] def blacken() 
    private[RBLinkedTree] def redden() 
    
    private[RBLinkedTree] def parent: TreeNode
    private[RBLinkedTree] def left: TreeNode
    private[RBLinkedTree] def right: TreeNode 
    
    private[RBLinkedTree] def parent_= (node : TreeNode)
    private[RBLinkedTree] def left_= (node : TreeNode)
    private[RBLinkedTree] def right_= (node : TreeNode)
    
    private[RBLinkedTree] def hasLeft: Boolean
    private[RBLinkedTree] def hasRight: Boolean
    private[RBLinkedTree] def hasParent: Boolean
  }
  
  class Empty extends TreeNode {
    private[RBLinkedTree] def isBlack: Boolean = true
    private[RBLinkedTree] def isEmpty: Boolean = true
    private[RBLinkedTree] def blacken() = new RuntimeException("empty node")
    private[RBLinkedTree] def redden() = new RuntimeException("empty node")
    
    private[RBLinkedTree] var left: TreeNode = this
    private[RBLinkedTree] var right: TreeNode = this
    private[RBLinkedTree] var parent: TreeNode = this
        
    private[RBLinkedTree] def hasLeft: Boolean = false
    private[RBLinkedTree] def hasRight: Boolean = false
    private[RBLinkedTree] def hasParent: Boolean = false
  }
  
  trait LinkedNode {  
    protected def isSentinel: Boolean
    private[RBLinkedTree] var n: LinkedNode = null
    private[RBLinkedTree] var p: LinkedNode = null   
    def hasNext = !n.isSentinel
    def hasPrev = !p.isSentinel     
    // Only for users (hide the sentinel node)   
    def next: RBNode
    def prev: RBNode
  }
  
  // Sentinel node used to maintain (easily) the LinkedList 
  case object Sentinel extends LinkedNode {
    protected def isSentinel = true
    def next = if (hasNext) toRBNode(n) else throw new NoSuchElementException("empty list")
    def prev = if (hasPrev) toRBNode(p) else throw new NoSuchElementException("empty list")
    def reset() {
      n = null
      p = null
    }
  }
  
  case class RBNode(key: Int, value: T) extends TreeNode with LinkedNode {  
    
    private[RBLinkedTree] def isEmpty = false
    protected def isSentinel = false
    
    private var black = false
    private[RBLinkedTree] def isBlack = black
    private[RBLinkedTree] def blacken() { black = true }
    private[RBLinkedTree] def redden() { black = false }
    
    private[RBLinkedTree] var left: TreeNode = empty
    private[RBLinkedTree] var right: TreeNode = empty
    private[RBLinkedTree] var parent: TreeNode = empty
    
    private[RBLinkedTree] def hasLeft: Boolean = !left.isEmpty
    private[RBLinkedTree] def hasRight: Boolean = !right.isEmpty
    private[RBLinkedTree] def hasParent: Boolean = !parent.isEmpty
    
    def next = if (hasNext) toRBNode(n) else throw new NoSuchElementException("next node is empty")
    def prev = if (hasPrev) toRBNode(p) else throw new NoSuchElementException("previous node is empty") 
  }
  
  private val empty = new Empty
  private var root: TreeNode = empty
  private var _size = 0
  
  def size = _size
  def isEmpty = size == 0
  def first: RBNode = Sentinel.next
  def last: RBNode = Sentinel.prev

  def insert(key: Int, value: T): RBNode = {
    if (!isEmpty) {
      _size += 1
      insertIn(key, value, toRBNode(root))    
    }
    else {
      _size += 1
      val newNode = RBNode(key, value)
      root = newNode
      newNode.blacken
      Sentinel.p = newNode
      newNode.n = Sentinel
      Sentinel.n = newNode
      newNode.p = Sentinel
      newNode
    }
  }
  
  def insertIn(key: Int, value: T, node: RBNode): RBNode = {   
    val newNode = RBNode(key, value)
    insert(node, newNode)
    balance(newNode)
    newNode
  }
  
  private def insert(node: TreeNode, newNode: RBNode) {
    val n = toRBNode(node)
    if (newNode.key < n.key) insertLeft(n, newNode)
    else insertRight(n, newNode)
  }
  
  private def insertLeft(node: RBNode, newNode: RBNode) {
    if (node.hasLeft) insert(node.left, newNode)
    else {
      val prev = node.p
      node.left = newNode
      newNode.parent = node
      newNode.p = prev
      prev.n = newNode
      newNode.n = node
      node.p = newNode
    }
  }
  
  private def insertRight(node: RBNode, newNode: RBNode) {
    if (node.hasRight) insert(node.right, newNode)
    else {
      val next = node.n
      node.right = newNode
      newNode.parent = node
      newNode.n = next
      next.p = newNode
      newNode.p = node
      node.n = newNode
    }
  }
  
  private def balance(node: TreeNode) {
    var z = node
    while (z.parent.isRed) {
      // If the node is red, it has a parent
      if (z.parent == z.parent.parent.left) {
        val y = z.parent.parent.right
        if (y.isRed) {
          z.parent.blacken()
          y.blacken()
          z.parent.parent.redden()
          z = z.parent.parent        
        }
        else {
          if (z == z.parent.right) {
            z = z.parent
            balanceLeft(z)
          }
          z.parent.blacken()
          z.parent.parent.redden()
          balanceRight(z.parent.parent)
        }      
      } 
      else if (z.parent == z.parent.parent.right) {
        val y = z.parent.parent.left
        if (y.isRed) {
          z.parent.blacken()
          y.blacken()
          z.parent.parent.redden()
          z = z.parent.parent        
        }
        else {
          if (z == z.parent.left) {
            z = z.parent
            balanceRight(z)
          }
          z.parent.blacken()
          z.parent.parent.redden()
          balanceLeft(z.parent.parent)
        }
      }
    }   
    // The root is always black
    root.blacken()
  }
  
  private def balanceLeft(x: TreeNode) {
    val y = x.right
    x.right = y.left
    if (!y.left.isEmpty) y.left.parent = x    
    y.parent = x.parent 
    if(x.parent.isEmpty) root = y
    else if (x == x.parent.left) x.parent.left = y
    else x.parent.right = y
    y.left = x
    x.parent = y
  }
  
  private def balanceRight(x: TreeNode) {
    val y = x.left
    x.left = y.right
    if (!y.right.isEmpty) y.right.parent = x    
    y.parent = x.parent 
    if(x.parent.isEmpty) root = y
    else if (x == x.parent.right) x.parent.right = y
    else x.parent.left = y
    y.right = x
    x.parent = y
  }
  
  private def transplant(u: TreeNode, v: TreeNode) {
    if (u.parent.isEmpty) root = v
    else if (u == u.parent.left) u.parent.left = v
    else u.parent.right = v
    v.parent = u.parent
  }
  
  def remove(z: RBNode) {
    _size -= 1
    removeLinkedNode(z)
    var y = z
    var yCol = y.isBlack
    if (z.left.isEmpty) {
      val x = z.right
      transplant(z, z.right)
      if (yCol) removeFixup(x)
    }
    else if (z.right.isEmpty) {
      val x = z.left
      transplant(z, z.left)
      if (yCol) removeFixup(x)
    }
    else {
      // z has a right and a left child
      y = treeMin(toRBNode(z.right))
      yCol = y.isBlack
      val x = y.right
      if (y.parent == z) x.parent = y
      else {
        transplant(y, y.right)
        y.right = z.right
        y.right.parent = y
      }
      transplant(z, y)
      y.left = z.left
      y.left.parent = y
      if (z.isBlack) y.blacken() else y.redden()
      if (yCol) removeFixup(x)
    }
  }
  
  private def removeFixup(node: TreeNode) {
    var x = node
    while (x != root && x.isBlack) {
      if (x == x.parent.left) {
        var w = x.parent.right
        if (w.isRed) {
          w.blacken()
          x.parent.redden()
          balanceLeft(x.parent)
          w = x.parent.right
        }
        if (w.left.isBlack && w.right.isBlack) {
          w.redden()
          x = x.parent
        }
        else {
          if (w.right.isBlack) {
            w.left.blacken()
            w.redden()
            balanceRight(w)
            w = x.parent.right
          }
          if (w.parent.isBlack) w.blacken() else w.redden()
          w.parent.blacken()
          w.right.blacken()
          balanceLeft(x.parent)
          x = root
        }
      }
      else if (x == x.parent.right) {
        var w = x.parent.left
        if (w.isRed) {
          w.blacken()
          x.parent.redden()
          balanceRight(x.parent)
          w = x.parent.left
        }
        if (w.right.isBlack && w.left.isBlack) {
          w.redden()
          x = x.parent
        }
        else {
          if (w.left.isBlack) {
            w.right.blacken()
            w.redden()
            balanceLeft(w)
            w = x.parent.left
          }
          if (w.parent.isBlack) w.blacken() else w.redden()
          w.parent.blacken()
          w.left.blacken()
          balanceRight(x.parent)
          x = root
        }
      }
    }
    x.blacken()
  }
  
  private def treeMin(node : RBNode): RBNode = {
    if (node.hasLeft) treeMin(toRBNode(node.left))
    else node
  }
  
  private def removeLinkedNode(node: RBNode) {
    if (node.hasPrev) node.prev.n = node.n
    else Sentinel.n = node.n
    if (node.hasNext) node.next.p = node.p
    else Sentinel.p = node.p
  }
  
  def removeAll() = {
    _size = 0
    root = empty
    Sentinel.reset()
  }
  
  def find(key: Int): RBNode = {
    if (!isEmpty) find(key, toRBNode(root))
    else throw new NoSuchElementException("empty list")
  }
  
  private def find(key: Int, node: RBNode): RBNode = {
    if (key < node.key && node.hasLeft) find(key, toRBNode(node.left))
    else if (key > node.key && node.hasRight) find(key, toRBNode(node.right))
    else node
  }
  
  def toList: List[T] = {
    if (isEmpty) List()
    else toList(first)
  }
  
  private def toList(node: RBNode): List[T] = {
    node.value :: (if (node.hasNext) toList(node.next) else Nil)
  }
  
  def toReversedList: List[T] = {
    if (isEmpty) List()
    else toReversedList(last)
  }
  
  private def toReversedList(node: RBNode): List[T] = {
    node.value :: (if (node.hasPrev) toReversedList(node.prev) else Nil)
  }
  
  private def toRBNode(node: LinkedNode): RBNode = node.asInstanceOf[RBNode]
  private def toRBNode(node: TreeNode): RBNode = node.asInstanceOf[RBNode]
  
  override def toString = size.toString
}

object RBLinkedTree {
  def main(args: Array[String]) = {
    
    val c = new RBLinkedTree[String]
    
    c insert (1, "A")
    c insert (2, "B")
    c insert (3, "C")
    c insert (4, "D")
    c insert (5, "E")    
    c insert (6, "F")
    c insert (7, "G")
    c insert (8, "H")
    c insert (9, "I")
    c insert (10, "J")    
    c insert (11, "K")
    c insert (12, "L")
    c insert (13, "M")
    c insert (14, "N")
    c insert (15, "O")    
    c insert (16, "P")
    
    println(c.toList)
    println(c.toReversedList)
    
    val node = c.find(8)
    c.remove(node)
    println(c.toList)
    println(c.size)
  }
}