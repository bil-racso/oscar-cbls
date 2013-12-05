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


package oscar.algo.search


import scala.collection.mutable.Stack
import scala.util.continuations._
import oscar.visual.VisualController
import oscar.algo.search.SearchNode
import oscar.algo.search.SearchController
import oscar.algo.search.SearchNode
import oscar.algo.search.SearchController
import oscar.algo.search.MyContinuation

/**
 * Non deterministic search controller as described in the wonderful paper:
 * "Non Deterministic Control for Hybrid Search" by Pascal Van Hentenryck and Laurent Michel.
 * In their paper they introduce the nicest possible way to implement a search for CP (in Comet).
 * @author Pierre Schaus pschaus@gmail.com & Sebastien Mouthuy smouthuy@gmail.com
 */
class DFSSearchController(node: SearchNode) extends SearchController(node) {

  val stack: Stack[MyContinuation] = new Stack()

  def addChoice(e: MyContinuation) {
    stack.push(e)
  }

  override def reset() {
    super.reset()
    stack.clear()
  }


  def explore(): Unit = {

    if (pausable && VisualController.inPause) {
      if (!stack.isEmpty && !limitReached && !exit) {
        stack.pop.call()
        VisualController.cont = Unit => { explore() }
      }
    } else {
      while (!stack.isEmpty && !limitReached && !exit) {
        stack.pop.call()
      }
    }
    
  }

}

