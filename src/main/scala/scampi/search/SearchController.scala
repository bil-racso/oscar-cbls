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


import scala.util.continuations._
import scampi.reversible.ReversibleSearchNode
import scala.collection.mutable.Stack



class MyContinuation(msg: String, block: => Unit)  {
      def call() = {
        block
      }
      override def toString = msg
}

/**
 * Non deterministic search controller as described in the wonderful paper:
 * "Non Deterministic Control for Hybrid Search" by Pascal Van Hentenryck and Laurent Michel.
 * In their paper they introduce the nicest possible way to implement a search for CP (in Comet).
 * @author Pierre Schaus pschaus@gmail.com
 */
abstract class SearchController(val node: ReversibleSearchNode) {
  
  
  
  var nbFail = 0
  
  var failLimit = Int.MaxValue
  
  var limitReached = false
  
  var exit = false
  
  def stop() {
    exit = true
  }
  
  def addChoice(e: MyContinuation)
  
  def start(): Unit@suspendable = {}
  
  def fail() { 
      nbFail += 1
      if (nbFail > failLimit) {
        limitReached = true
      }
  }
  
  def reset() {
    limitReached = false
    nbFail = 0
  }
  
  
  def explore()

}

