package oscar.util.tree

import java.awt.Color

class Node[T](val label: T, val sons: List[Node[T]], val edgeLabels: List[T], val col: Color= Color.white) {
	
	override def toString = {
	  label.toString + (sons match {
	    case Nil => ""
	    case e => e.mkString("(",",",")")
	  })
	}
}

object Node {
  def apply[T](label: T, sons: List[Node[T]], edgeLabels: List[T], col: Color = Color.white) = new Node(label, sons, edgeLabels)
  def apply[T](label: T, col: Color = Color.white) = new Node(label, List[Node[T]](), List[T](),col)
  
  def design[T](tree: Node[T], minDist: Double = 2): PositionedNode[T] = {
    val formerMinDist = Extent.minDist
    Extent.minDist = minDist
    def designAux(node: Node[T]): (PositionedNode[T], Extent) = {
      val (trees: List[PositionedNode[T]], extents: List[Extent]) = node.sons.map(subTree => designAux(subTree)).unzip
      val positions = Extent.fitList(extents)
      val pTrees = trees.zip(positions).map(e => e._1.moveTree(e._2))
      val pExtents = extents.zip(positions).map(e => e._1.moveExtent(e._2))
      val resultExtent = Extent ((0.0, 0.0) :: Extent.mergeList(pExtents).extentList)
      val resultTree = PositionedNode(node.label, 0.0, pTrees, node.edgeLabels, node.col)
      (resultTree, resultExtent)
    }
    val treeToRet = designAux(tree)._1
    Extent.minDist = formerMinDist
    treeToRet
  }
}