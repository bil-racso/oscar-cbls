package oscar.lcg.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.TrailEntry
import oscar.algo.array.ArrayStackInt
import oscar.algo.array.ArrayQueueInt
import oscar.lcg.variables.IntVar

class LCGStore {
  
  // Trail
  private[this] val trailQueue = new ReversibleContext()
  
  // Conflict analyzer
  private[this] val analyzer = new ConflictAnalyzer()
  
  // Registered literals
  private[this] val variables = new ArrayStack[IntVar](128)
  
  // Propagation queues
  private[this] val nogoodsQueue = new ArrayQueue[Literal](128)
  private[this] val constraintsQueue = new ArrayQueue[Constraint](128)
  
  def trail: ReversibleContext = trailQueue
  
  def newLevel(): Unit = {
    trail.pushState()
    analyzer.newLevel()
  }
  
  def undoLevel(): Unit = {
    trail.pop()
    analyzer.undoLevel()
  }

  def enqueue(literal: Literal): Unit = {
    assert(literal.isAssigned)
    if (literal.isFalse) nogoodsQueue.addLast(literal)
    else nogoodsQueue.addLast(literal.opposite)
  }
  
  def enqueue(constraint: Constraint): Unit = {
    constraintsQueue.addLast(constraint)
  }

  protected def propagate(): Clause = {
    if (fixedPoint()) null
    else {
      // analyze
      // Build the clause
      // return it
      ???
    }
  }
  
  @inline private def fixedPoint(): Boolean = {
    var noConflict = true 
    while (noConflict && (!nogoodsQueue.isEmpty || !constraintsQueue.isEmpty)) {     
      // Propagate nogoods
      while (noConflict && !nogoodsQueue.isEmpty) {
        val lit = nogoodsQueue.removeFirst()
        val clauses = lit.clauses
        var i = clauses.size
        while (i > 0 && noConflict) {
          i -= 1
          val clause = clauses.removeFirst()
          noConflict = clause.propagate()
        }
      }    
      // Propagate constraints
      while (noConflict && nogoodsQueue.isEmpty && !constraintsQueue.isEmpty) {
        val constraint = constraintsQueue.removeFirst()
        noConflict = constraint.propagate()
      }
    }   
    if (noConflict) true
    else {
      nogoodsQueue.clear()
      constraintsQueue.clear()
      false
    }
  }
}
