/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.search


import scampi.reversible._
import scala.collection.mutable.Stack
import scala.util.continuations._



/**
 * Controller for iterative discrepancy search
 * @author Pierre Schaus pschaus@gmail.com & Sebastien Mouthuy smouthuy@gmail.com
 */
class IDSSearchController(node: ReversibleSearchNode, val maxDiscrepency: Int) extends SearchController(node) {
  
  val stack: Stack[MyContinuation] = new Stack()
  val discr = new ReversibleInt(node,0)
  val inDFS = new ReversibleBool(node,false)
  var maxDiscr = 10
  
  def addChoice(e: MyContinuation) { 
	node.pushState()
    stack.push(e)
  } 
  
  
  override def reset() {
    super.reset()
    stack.clear()
  }
  
  override def start() = {
    discr.setValue(0)
    node.branchAll(0 to maxDiscrepency) {i =>
      discr.setValue(0)
      maxDiscr = i // max discrepancy of this DFS search = i
      println("discrepency = "+i)
    }
    inDFS.setValue(true)
  }
  
  override def fail() {
    super.fail()
    discr.incr() // increment the number of discrepancy
    if (inDFS.getValue() &&  discr.getValue() > maxDiscr) {
      node.fail()
    }
  }
  
  
  def explore() {
    while(!stack.isEmpty && !limitReached) {
      node.pop()
      fail()
      stack.pop.call()
      
    }
  }

}

