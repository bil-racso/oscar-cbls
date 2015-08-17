package oscar.lcg.variables

import oscar.lcg.core.LCGStore
import oscar.lcg.core.Literal
import oscar.lcg.core.Constraint
import oscar.algo.reversible.TrailEntry
import oscar.algo.array.ArrayStack
import oscar.lcg.literals.LitBooleanTrue

final class BooleanVarImpl(override val store: LCGStore, override val name: String) extends BooleanVar with TrailEntry {

  private[this] val _eqLit: Literal = new LitBooleanTrue(this)
  private[this] val trail = store.trail
    
  // Registered constraints 
  private[this] val constraints = new ArrayStack[Constraint](4)

  // Domain 
  // 2: unassigned
  // 1: true
  // 0: false
  private[this] var _domain: Int = 2

  override def isAssigned: Boolean = _domain != 2

  override def isTrue: Boolean = _domain == 1

  override def isFalse: Boolean = _domain == 0

  override def assignTrue(explanation: Array[Literal]): Boolean = {
    if (_domain == 2) {
      trail.trail(this)
      _domain = 1
      notifyConstraints()
      true
    } else _domain == 1
  }

  override def assignFalse(explanation: Array[Literal]): Boolean = {
    if (_domain == 2) {
      trail.trail(this)
      _domain = 0
      notifyConstraints()
      true
    } else _domain == 0
  }

  override def awakeOnAssign(constraint: Constraint): Unit = {
    constraints.push(constraint)
  }

  override def restore(): Unit = _domain = 2

  override val eqLit: Literal = _eqLit

  override val diffLit: Literal = _eqLit.opposite

  @inline private def notifyConstraints(): Unit = {
    var i = constraints.length
    while (i > 0) { i -= 1; store.enqueue(constraints(i)) }
  }
}