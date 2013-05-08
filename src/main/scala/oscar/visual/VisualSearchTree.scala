/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.visual;

import java.awt.Shape
import oscar.reversible.ReversibleSearchNode
import oscar.util.tree.Node
import javax.swing._
import java.awt.BorderLayout

/**
 * Class to show a visual search tree (currently only available for binary search trees)
 * @author Pierre Schaus pschaus@gmail.com
 */

class VisualSearchTree(node: ReversibleSearchNode) extends JPanel() {

  
  var root = Node.design(node.tree.toNode(0), 42)
  var visualTree = new VisualLabelledTree(root) 
  
  add(visualTree)

  def update() {
    root = Node.design(node.tree.toNode(0), 42)
    visualTree.update(root)
  }

}

object VisualSearchTree {
  def main(args: Array[String]) {

  }
}
