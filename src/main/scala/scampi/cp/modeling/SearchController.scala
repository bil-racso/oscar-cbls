package scampi.cp.modeling


import scampi.reversible.ReversibleSearchNode
import scala.collection.mutable.Stack
import scampi.search._



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
 * @author Pierre Schaus pschaus@gmail.com & Sebastien Mouthuy smouthuy@gmail.com
 */
class SearchController(val node: ReversibleSearchNode) {
  
  val stack: Stack[MyContinuation] = new Stack()
  
  var nbFail = 0
  
  var failLimit = Int.MaxValue
  
  var limitReached = false
  
  def addChoice(e: MyContinuation) { 
	node.pushState()
    stack.push(e)
  } 
  
  def fail() { 
      nbFail += 1
      if (nbFail > failLimit) {
        limitReached = true
      }
  }
  
  def reset() {
    stack.clear()
    limitReached = false
    nbFail = 0
  }
  
  
  def explore() {
    while(!stack.isEmpty && !limitReached) {
      node.pop()
      stack.pop.call()
      fail()
    }
  }

}

