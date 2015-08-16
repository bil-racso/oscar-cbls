package oscar.lcg.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.TrailEntry
import oscar.algo.array.ArrayStackInt
import oscar.algo.array.ArrayQueueInt
import oscar.lcg.variables.IntVar
import oscar.algo.reversible.ReversibleBoolean

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
  
  // State of the solver
  private[this] val _failed = new ReversibleBoolean(trail, false)
  
  def trail: ReversibleContext = trailQueue
  
  def isFailed = _failed.value
  
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
    if (constraint.isEnqueuable) {
      constraint.enqueued = true
      constraintsQueue.addLast(constraint)     
    }
  }

  def propagate(): Boolean = {
    if (fixedPoint()) true
    else {
      // Change store status 
      _failed.setTrue()
      // Clean queues
      nogoodsQueue.clear()
      constraintsQueue.clear()
      // analyze
      // Build the clause
      // return/add it
      false
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
        constraint.enqueued = false
        noConflict = constraint.propagate()
      }
    }   
    noConflict
  }
}
