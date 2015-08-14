package oscar.lcg.variables

import oscar.algo.array.ArrayStack
import oscar.algo.reversible.TrailEntry
import oscar.lcg.core.LCGStore
import oscar.lcg.core.Literal
import oscar.lcg.core.Constraint
import oscar.lcg.literals.LitTrue
import oscar.lcg.literals.LitFalse

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
    assert(value > _min)
    val oldMin = _min
    // Update domain
    trail.trail(new TrailMin(_min))
    _min = value
    // Notify clauses
    val lit = literals(value - initMin - 1)
    store.enqueue(lit) // notify clauses of the input literal 
    notifyLiteralLeq(oldMin, value) // notify clauses of implied literals   
    // Notify constraints
    var i = constraints.length
    while (i > 0) { i -= 1; store.enqueue(constraints(i)) }
    // End
    true
  }
  
  def updateMaxByLit(value: Int): Boolean = {
    assert(value >= _min)
    assert(value < _max)
    val oldMax = _max
    // Update domain
    trail.trail(new TrailMax(_max))
    _max = value
    // Notify clauses
    val lit = literals(value - initMin)
    store.enqueue(lit) // notify clauses of the input literal 
    notifyLiteralGeq(oldMax, value) // notify clauses of implied literals   
    // Notify constraints
    var i = constraints.length
    while (i > 0) { i -= 1; store.enqueue(constraints(i)) }
    // End
    true
  }
  
  // Notify and explain all unassigned literals lower than value
  @inline private def notifyLiteralLeq(oldMin: Int, value: Int): Unit = {
    var i = value - initMin
    val n = oldMin - initMin
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
    
  // Notify and explain all unassigned literals lower than value
  @inline private def notifyLiteralGeq(oldMax: Int, value: Int): Unit = {
    var i = value - initMin
    val n = oldMax - initMin
    var prec = literals(i)
    while (i > n) {
      i -= 1
      val lit = literals(i)
      // Explain with its predecessor
      literals(i).opposite.explain(prec)
      // Enqueue literal
      store.enqueue(lit)
      prec = lit
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
    if (value <= initMax) LitTrue
    else if (value < initMin) LitFalse
    else literals(value - initMin)
  }
  
  def geqLit(value: Int): Literal = {
    if (value >= initMin) LitTrue
    else if (value > initMax) LitFalse
    else literals(value - initMin - 1).opposite
  }
  
  @inline private def buildDomain(): Array[Literal] = {
    ???
  }
}