package oscar.lcg.variables

import oscar.algo.array.ArrayStack
import oscar.algo.reversible.TrailEntry
import oscar.lcg.core.LCGStore
import oscar.lcg.core.Literal
import oscar.lcg.core.Constraint

final class IntVarImpl(store: LCGStore, initMin: Int, initMax: Int, name: String) {
  
  // Trailables used to restore the domain state
  final class TrailMax(oldMax: Int) extends TrailEntry { override def restore(): Unit = _max = oldMax }
  final class TrailMin(oldMin: Int) extends TrailEntry { override def restore(): Unit = _min = oldMin }
  private[this] val trail = store.trail
  
  // Domain representation
  private[this] val initSize = initMax - initMin + 1
  private[this] var _min = initMin
  private[this] var _max = initMax
    
  // Literals
  private[this] val literals = buildDomain()
  private[this] val nLiterals = literals.length
  
  // Registered constraints 
  private[this] val constraints = new ArrayStack[Constraint](8)
  
  def min: Int = _min
  
  def max: Int = _max
  
  def size: Int = _max - _min
  
  def contains(value: Int): Boolean = _min <= value && value <= _max
  
  def isAssigned: Boolean = _max == _min
  
  def isAssignedTo(value: Int): Boolean = _min == value && _max == value
  
  def updateMinByLit(value: Int): Boolean = {
    assert(value <= _max)
    val lit = literals(value - initMin - 1)
    store.enqueue(lit)
    // Notify literals
    // Notify constraints
    var i = constraints.length
    while (i > 0) { i -= 1; store.enqueue(constraints(i)) }
    // Trail domains
    trail.trail(new TrailMin(_min))
    _min = value
    // End
    true
  }
  
  // Notify and explain all unassigned literals lower than value
  @inline private def notifyLiteralLeq(value: Int): Unit = {
    var i = value - initMin
    val n = _min - initMin
    var succ = literals(i)
    while (i > n) {
      i -= 1
      val lit = literals(i)
      // Explain with its successor
      literals(i).opposite.explain(succ)
      // Enqueue literal
      store.enqueue(lit)
      succ = lit
    }
  }
  
  def updateMin(value: Int, explanation: Array[Literal]): Boolean = {
    if (value <= _min) true
    else if (value > _max) {
      fail(value, explanation)
      false
    } else {
      val litId = value - initMin
      val lit = literals(litId)
      // Explain literal
      lit.explain(explanation)
      // Notify clauses and enqueue literals
      // TODO
      // Notify constraints
      var i = constraints.length
      while (i > 0) { i -= 1; store.enqueue(constraints(i)) }
      // Trail domains
      trail.trail(new TrailMin(_min))
      _min = value
      // End
      true
    }
  }
  
  def updateMax(value: Int, explanation: Array[Int]): Boolean = {
    if (value >= _max) true
    else if (value < _min) {
      fail(value, explanation)
      false
    } else {
      val litId = value - initMin
      val lit = literals(litId)
      // Explain literal
      store.
      // Notify clauses and enqueue literals
      // TODO
      // Notify constraints
      var i = constraints.length
      while (i > 0) { i -= 1; store.enqueue(constraints(i)) }
      // Trail domains
      trail.trail(new TrailMax(_max))
      _max = value
      // End
      true
    }
  }
  
  def awakeOnChanges(constraint: Constraint): Unit = constraints.append(constraint)

  @inline private def fail(value: Int, explanation: Array[Literal]): Unit = {
    
    // store.explain(lit, explanation)
  }
  
  def leqLit(value: Int): Literal = {
    /*val id = value - initMin
    if (id >= nLiterals) store.trueLiteral
    else if (id < 0) store.falseLiteral
    else literals(id)*/
    ???
  }
  
  def geqLit(value: Int): Literal = {
    /*val id = value - initMin - 1
    if (id < 0) store.trueLiteral
    else if (id >= nLiterals) store.falseLiteral
    else literals(id).opposite*/
    ???
  }
  
  @inline private def buildDomain(): Array[Literal] = {
    ???
  }
}