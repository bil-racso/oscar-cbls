package oscar.cp.mem.pareto

class RedBlackTreeList[T] {
  
  private val empty = RBNode(0, None)
    
  private var size = 0   
  
  private var first: RBNode = empty
  private var last: RBNode = empty 
  private var root: RBNode = empty
  
  abstract class Color
  object Red extends Color
  object Black extends Color
  
  case class RBNode(key: Int, v: Option[T]) {  
    var value: Option[T] = v
    // Red and Black Tree
    var color: Color = Red
    var left: RBNode = empty
    var right: RBNode = empty
    var parent: RBNode = empty
    // Linked-List
    var succ: RBNode = empty
    var pred: RBNode = empty
  }
  
  private def bindLeft(l: RBNode, p: RBNode) {
    val t = p.pred    
    t.succ = l
    l.pred = t
    l.succ = p
    p.pred = l
  }
  
  private def bindRight(r: RBNode, p: RBNode) {
    val t = p.succ
    t.pred = r
    r.succ = t
    r.pred = p
    p.succ = r
  }
  
  private def bind(p: RBNode, s: RBNode) {
    p.succ = s
    s.pred = p
  }
  
  def insert(key: Int, value: T) = insertIn(key, value, root)
  
  def insertIn(key: Int, value: T, node: RBNode) = {
    size += 1   
    val z = RBNode(key, Some(value))  
    var x = node
    var y: RBNode = empty
    while(x != empty) {
      y = x
      if (z.key < x.key) x = x.left
      else x = x.right
    }
    z.parent = y
    
    if (y == empty) {
      first = z 
      last = z
      root = z
    }
    else if (z.key < y.key) {
      y.left = z
      bindLeft(z, y)
    }
    else {
      y.right = z
      bindRight(z, y)
    }
    
    insertFixUp(z)
  }
  
  private def insertFixUp(x: RBNode) {
    var z = x
    while (z.parent.color == Red) {     
      if (z.parent == z.parent.parent.left) {        
        val y = z.parent.parent.right       
        if (y.color == Red) {
          z.parent.color = Black
          y.color = Black
          z.parent.parent.color = Red
          z = z.parent.parent
        }
        else {
          if (z == z.parent.right) {       
            z = z.parent
            leftRotate(z)
          }
          z.parent.color = Black
          z.parent.parent.color = Red
          rightRotate(z.parent.parent)
        }
      } 
      else {
        val y = z.parent.parent.left      
        if (y.color == Red) {
          z.parent.color = Black
          y.color = Black
          z.parent.parent.color = Red
          z = z.parent.parent
        }
        else {
          if (z == z.parent.left) {       
            z = z.parent
            rightRotate(z)
          }
          z.parent.color = Black
          z.parent.parent.color = Red
          leftRotate(z.parent.parent)
        }
      }     
    } 
    root.color = Black
  }
  
  private def leftRotate(x: RBNode) {
    val y = x.right
    x.right = y.left
    if (y.left != empty) y.left.parent = x
    y.parent = x.parent
    if (x.parent == empty) root = y
    else if (x == x.parent.left) x.parent.left = y
    else x.parent.right = y
    y.left = x
    x.parent = y
  }

  private def rightRotate(x: RBNode) {
    val y = x.left
    x.left = y.right
    if (y.right != empty) y.right.parent = x
    y.parent = x.parent
    if (x.parent == empty) root = y
    else if (x == x.parent.right) x.parent.right = y
    else x.parent.left = y
    y.right = x
    x.parent = y
  }
  
  private def transplant(u: RBNode, v: RBNode)  {
    if (u.parent == empty) root = v
    else if (u == u.parent.left) u.parent.left = v
    else u.parent.right = v
    v.parent = u.parent
  }
  
  def delete(z: RBNode) {  
    
    size -= 1
    if (size > 0) bind(z.pred, z.succ)
    else {
      first = empty
      last = empty
    }
    
    var y = z
    var x = z
    var col = y.color
    
    if (z.left == empty) {
      x = z.right
      transplant(z, z.right)
    }
    else if (z.right == empty) {
      x = z.left
      transplant(z, z.left)
    }
    else {
      y = min(z.right)
      col = y.color
      x = y.right
      if (y.parent == z) x.parent == y
      else {
        transplant(y, y.right)
        y.right = z.right
        y.right.parent = y
      }
      transplant(z, y)
      y.left = z.left
      y.left.parent = y
      y.color = z.color
    }
    
    if (col == Black) deleteFixUp(x)
  }
  
  private def deleteFixUp(z: RBNode) {
    var x = z
    while (x != empty && x.color == Black) {
      if (x == x.parent.left) {
        var w = x.parent.right
        if (w.color == Red) {
          w.color = Black
          x.parent.color = Red
          leftRotate(x.parent)
          w = x.parent.right
        }
        if (w.left.color == Black && w.right.color == Black) {
          w.color == Red
          x = x.parent
        }
        else {
          if (w.right.color == Black) {
            w.left.color = Black
            w.color = Red
            rightRotate(w)
            w = x.parent.right
          }
          w.color = x.parent.color
          x.parent.color = Black
          w.right.color = Black
          leftRotate(x.parent)
          x = root
        }
      }
      else {
        var w = x.parent.left
        if (w.color == Red) {
          w.color = Black
          x.parent.color = Red
          rightRotate(x.parent)
          w = x.parent.left
        }
        if (w.right.color == Black && w.left.color == Black) {
          w.color == Red
          x = x.parent
        }
        else {
          if (w.left.color == Black) {
            w.right.color = Black
            w.color = Red
            leftRotate(w)
            w = x.parent.left
          }
          w.color = x.parent.color
          x.parent.color = Black
          w.left.color = Black
          rightRotate(x.parent)
          x = root
        }
      }
    }
    x.color = Black
  }
  
  private def min(x: RBNode): RBNode = {
    if (x.left == empty) x
    else min(x.left)
  }
}

object RedBlackTreeList extends App {
  
  val tree = new RedBlackTreeList[String]()
  tree.insert(1, "A")
  tree.insert(2, "B")
  tree.insert(3, "C")
  tree.insert(4, "D")
  tree.insert(5, "E")
}