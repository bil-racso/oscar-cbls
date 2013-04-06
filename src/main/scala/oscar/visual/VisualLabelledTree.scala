package oscar.visual

import oscar.util.tree.Node
import oscar.util.tree.PositionedNode
import oscar.util.tree.Extent

class VisualLabelledTree[T](var tree: PositionedNode[T]) extends VisualDrawing(true) {
  
  private def levelHeight = 4 * this.getFontMetrics(this.getFont()).getHeight()
  private def baseOffset = this.getFontMetrics(this.getFont()).stringWidth(tree.label.toString) + tree.minOffset
  var rectSet = Set[VisualLabelledRectangle]()
  var branchSet = Set[VisualLabelledBranch]()
  getRectangles
  
  def this(tree: Node[T]) = {
    this(Node.design(tree))
  }
  
  def getRectangles = {
    def rectAux(node: PositionedNode[T], accOffset: Double, level: Int): Unit = {
      val newNode = new VisualLabelledRectangle(this, accOffset + node.pos, level * levelHeight, node.label.toString, 10)
      rectSet += newNode
      for (i <- 0 until node.sons.length) {
        branchSet += new VisualLabelledBranch(this,
            accOffset + node.pos + newNode.width / 2,
            level * levelHeight + newNode.height,
            accOffset + node.pos + node.sons(i).pos + newNode.getWidth(node.sons(i).label.toString) / 2,
            (level + 1) * levelHeight , node.edgeLabels(i).toString)
        rectAux(node.sons(i), accOffset + node.pos, level + 1)
      }
    }
    rectAux(tree, baseOffset, 0)
  }
  
  def replaceTree(newTree: PositionedNode[T]) = {
    this.removeAllShapes
    this.tree = newTree
    update
  }
  
  def update = {
    //this.removeAll()
    rectSet.empty
    branchSet.empty
    getRectangles
    repaint()
  }
}

object VisualLabelledTree{
  	
  def main(args : Array[String]) {
	val f = new VisualFrame("toto");
	val inf = f.createFrame("Drawing");
	
	val C = Node("C")
	val D = Node("D")
	val E = Node("E")
	val H = Node("H")
	val I = Node("I")
	val J = Node("J")
	val B = Node("B", List(C, D, E), List("Son 1", "Son 2", "Son 3"))
	val F = Node("F")
	val G = Node("G", List(H, I, J), List("Son 1", "Son 2", "Son 3"))
	val A = Node("A", List(B, F, G), List("Son 1", "Son 2", "Son 3"))
	val positionedA = Node.design(A, 42)
	
	val visualTree = new VisualLabelledTree(positionedA);
	
	inf.add(visualTree);
	f.pack();
	
	visualTree.repaint()
			
	Thread.sleep(100000);
  }
}