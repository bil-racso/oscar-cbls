package scampi.cp.modeling


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
class SearchController(val node: ReversibleSearchNode) {
  
  val stack: Stack[MyContinuation] = new Stack()
  
  var exitCont: MyContinuation = null
  
  var nbFail = 0


  def start(e: MyContinuation) { exitCont = e } 
  def exit() = { exitCont.call() }
  
  def addChoice(e: MyContinuation) { 
	node.pushState()
    stack.push(e)
  } 
  
  def fail() { 
    if (stack.isEmpty) exit() 
    else {
      nbFail += 1
      node.pop()
      stack.pop().call()
    }
    
  }

}

