package oscar.cp.lcg.core

import oscar.algo.ArrayStack

class Clause(store: LCGStore, literals: Array[Literal], learnt: Boolean) {

  private[this] var _activity: Double = 0
  private[this] var _activate: Boolean = true
  
  /** Return the activity of the clause. */
  @inline final def activity: Double = _activity
  
  /** Change the activity of the clause. */
  final def activity_=(a: Double): Unit = _activity = a
  
  // UNKNOWN
  def locked: Boolean = store.assignReason(literals(0).varId) == this

  // UNKNOWN
  def remove(): Unit = Unit

  // UNKNOWN
  def simplify(): Boolean = true
  
  /** Return true if the clause is active. */
  @inline final def isActive: Boolean = _activate
  
  /** Kills the clause. */
  final def deactive(): Unit = _activate = false
  
  final def explain(outReason: ArrayStack[Literal]): Unit = {
    var i = 1
    while (i < literals.length) {
      outReason.append(literals(i).opposite)
      i += 1
    }
    if (learnt) store.claBumpActivity(this)
  }
  
  final def explainAll(outReason: ArrayStack[Literal]): Unit = {
        var i = 0
    while (i < literals.length) {
      outReason.append(literals(i).opposite)
      i += 1
    }
    if (learnt) store.claBumpActivity(this)
  }

  final def propagate(literal: Literal): Boolean = {
    // Make sure the false literal is literals(1)
    if (literals(0) == literal.opposite) {
      literals(0) = literals(1)
      literals(1) = literal.opposite
    }

    // If 0th watch is true, then clause is already satisfied
    if (store.value(literals(0)) == True) {
      store.watch(this, literal)
      return true
    }

    // Look for a new literal to watch
    var i = 2
    while (i < literals.length) {
      if (store.value(literals(i)) != False) {
        literals(1) = literals(i)
        literals(i) = literal.opposite
        store.watch(this, literals(1).opposite)
        return true
      }
      i += 1
    }
    
    // Clause is unit under assignment
    store.watch(this, literal)
    store.enqueue(literals(0), this)
  }
  
  final override def toString: String = literals.mkString("(", " ", ")")
}