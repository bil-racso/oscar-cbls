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


import scampi.reversible.ReversibleSearchNode
import scala.collection.mutable.Stack
import scala.util.continuations._




/**
 * Non deterministic search controller as described in the wonderful paper:
 * "Non Deterministic Control for Hybrid Search" by Pascal Van Hentenryck and Laurent Michel.
 * In their paper they introduce the nicest possible way to implement a search for CP (in Comet).
 * @author Pierre Schaus pschaus@gmail.com & Sebastien Mouthuy smouthuy@gmail.com
 */
class DFSSearchController(node: ReversibleSearchNode) extends SearchController(node) {
  
  val stack: Stack[MyContinuation] = new Stack()
  
  def addChoice(e: MyContinuation) { 
	node.pushState()
    stack.push(e)
  } 
  
  
  override def reset() {
    super.reset()
    stack.clear()
  }
  
  
  def explore() {
    while(!stack.isEmpty && !limitReached) {
      node.pop()
      fail()
      stack.pop.call()
    }
  }

}

