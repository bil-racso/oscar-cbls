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

package oscar.algo

import scala.collection.mutable.ArrayBuffer


class CCTreeNode(private[algo] var parent: Int, private[algo] var left: Int, private[algo] var right: Int, val index: Int) {
  private[algo] var v: Int = -1
  private[algo] var h: Int = 0
  def height = h
  
  private def reset() {
    parent = -1
    left = -1
    right = -1
  }
  def value = v
  def hasLeft = left >= 0
  def hasRight = right >= 0
  def hasParent = parent >= 0
}

/**
 * Connected Component Tree
 * @author Pierre Schaus pschaus@gmail.com
 * @param n the number of leaf nodes
 */
class CCTree(n: Int) {
  private var index = n
  private var rooted = false

  
  def reset() {
    index = n
    rooted = false
  }
  

  val nodes = Array.tabulate(n + n - 1)(i => new CCTreeNode(-1, -1, -1, i))
  val nodesInorder = Array.tabulate(n + n - 1)(i => nodes(i))
  def singleRoot = rooted

  def merge(left: CCTreeNode, right: CCTreeNode, value: Int): CCTreeNode = {
    assert(left.index < index && right.index < index)
    val parent = nodes(index)
    parent.left = left.index
    parent.right = right.index
    parent.v = value
    parent.parent = -1
    left.parent = index
    right.parent = index
    index += 1
    rooted = (index == nodes.length)
    if (rooted) computeHeights()
    parent
  }
  
  private def computeHeights() {
    height(root)
    def height(n : CCTreeNode) {
      n.h = if (n.hasParent) nodes(n.parent).h + 1 else 0
      if (n.hasLeft) height(nodes(n.left))
      if (n.hasRight) height(nodes(n.right))
    }
  }

  def root: CCTreeNode = {
    assert(index == nodes.length + 1)
    nodes(nodes.length-1)
  }

  /**
   * return the nodes sorted according to an inorder visit
   */
  def inorderCollect(): Array[CCTreeNode] = {
    var r = root
    var i = 0
    inorder(root)
    def inorder(n: CCTreeNode): Unit = {
      if (n.left != -1) inorder(nodes(n.left))
      nodesInorder(i) = n
      i += 1
      if (n.right != -1) inorder(nodes(n.right))
    }
    assert(i == index)
    nodesInorder
  }

}