package oscar.lcg.core

import oscar.algo.array.ArrayQueue
import oscar.algo.array.ArrayStack
import oscar.algo.reversible.ReversibleContext
import oscar.algo.reversible.TrailEntry
import oscar.algo.array.ArrayStackInt
import oscar.lcg.variables.IntVar
import oscar.algo.reversible.ReversibleBoolean

class LCGStore(val analyzer: ConflictAnalyzer) {

  def this() = this(new ConflictAnalyzer)

  // Trail
  private[this] val trailQueue = new ReversibleContext()

  // Conflict analyzer
  private[this] var _failedLiteral: Literal = null

  private[this] var _level = 0

  // Registered literals
  private[this] val variables = new ArrayStack[IntVar](128)

  // Propagation queues
  private[this] val nogoodsQueue = new ArrayQueue[Literal](128)
  private[this] val constraintsQueue = new ArrayQueue[Constraint](128)

  // Facts 

  // State of the solver
  private[this] val _failed = new ReversibleBoolean(trail, false)

  def trail: ReversibleContext = trailQueue

  def isFailed = _failed.value

  def failedLiteral_=(lit: Literal): Unit = _failedLiteral = lit

  def failedLiteral: Literal = _failedLiteral

  def level: Int = _level

  def add(constraint: Constraint): Boolean = {
    if (constraint.setup()) propagate()
    else {
      _failed.setTrue()
      false
    }
  }
  
  def add(clause: Clause): Boolean = {
    if (clause.setup()) propagate()
    else {
      _failed.setTrue()
      false
    }
  }

  def newLevel(): Unit = {
    _level += 1
    trail.pushState()
    analyzer.newLevel()
  }

  def undoLevel(): Unit = {
    _level -= 1
    trail.pop()
    analyzer.undoLevel()
  }

  def explained(literal: Literal): Unit = {
    analyzer.isExplained(literal)
  }

  def enqueue(literal: Literal): Unit = {
    assert(literal.isAssigned)
    if (literal.isTrue) nogoodsQueue.addLast(literal)
    else nogoodsQueue.addLast(literal.opposite)
  }

  def enqueue(constraint: Constraint): Unit = {
    if (constraint.isEnqueuable) {
      constraint.enqueued = true
      constraintsQueue.addLast(constraint)
    }
  }

  def propagate(): Boolean = {
    if (_failed.value) false
    else if (fixedPoint()) true
    else {
      // Change store status 
      _failed.setTrue()
      // Clear queues
      clearQueues()
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
          noConflict = clause.propagate(lit)
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

  @inline private def clearQueues(): Unit = {
    nogoodsQueue.clear()
    var i = constraintsQueue.size
    while (!constraintsQueue.isEmpty) {
      val constraint = constraintsQueue.removeFirst()
      constraint.enqueued = false
    }
  }
}
