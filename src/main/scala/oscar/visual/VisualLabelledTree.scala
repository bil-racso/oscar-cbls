/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.visual

import oscar.util.tree.Node
import oscar.util.tree.PositionedNode
import oscar.util.tree.Extent
import javax.swing.SwingUtilities

class VisualLabelledTree[T](var tree: PositionedNode[T]) extends VisualDrawing(false, false) {
  
  private def levelHeight = 4 * this.getFontMetrics(this.getFont()).getHeight()
  private def baseOffset = this.getFontMetrics(this.getFont()).stringWidth(tree.label.toString) + tree.minOffset
  var rectSet = Set[VisualLabelledRectangle]()
  var branchSet = Set[VisualLabelledBranch]()
  getRectangles
  
  
  def this(tree: Node[T]) = {
    this(Node.design(tree))
  }

  def update(t: PositionedNode[T]) {
    super.update
    SwingUtilities.invokeLater(new Runnable() {
      def run() {
        tree = t
        clear()
        rectSet = Set()
        branchSet = Set()
        getRectangles
        revalidate()
        repaint()
        repaint(0, 0, 2000, 2000)

      }
    })

  }
  
  def getRectangles = {
    def rectAux(node: PositionedNode[T], accOffset: Double, level: Int):VisualLabelledRectangle = {
      val newNode = new VisualLabelledRectangle(this, accOffset + node.pos, level * levelHeight, node.label.toString, 10)
      newNode.innerCol = node.col
      rectSet += newNode
      for (i <- 0 until node.sons.length) {
        branchSet += new VisualLabelledBranch(this,
            accOffset + node.pos + newNode.width / 2,
            level * levelHeight + newNode.height,
            accOffset + node.pos + node.sons(i).pos + newNode.getWidth(node.sons(i).label.toString) / 2,
            (level + 1) * levelHeight , node.edgeLabels(i).toString)
        rectAux(node.sons(i), accOffset + node.pos, level + 1).toolTip = node.edgeLabels(i).toString
      }
      newNode
    }
    rectAux(tree, baseOffset, 0)
  }
  
  def replaceTree(newTree: PositionedNode[T]) = {
    //this.removeAllShapes
    this.tree = newTree
    update
  }
  
  override def update = {
    super.update
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
	println(A)
	val positionedA = Node.design(A, 42)
	
	val visualTree = new VisualLabelledTree(positionedA);
	
	inf.add(visualTree);
	f.pack();
	
	visualTree.repaint()
			
	Thread.sleep(100000);
  }
}
