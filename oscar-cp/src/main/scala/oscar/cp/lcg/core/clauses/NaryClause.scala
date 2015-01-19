package oscar.cp.lcg.core.clauses

import oscar.cp.lcg.core.LCGStore
import oscar.algo.ArrayStack
import oscar.cp.lcg.core.Literal
import oscar.cp.lcg.core.True
import oscar.cp.lcg.core.False

class NaryClause(store: LCGStore, literals: Array[Literal], learnt: Boolean) extends Clause {
  
  final override def explainUnit(outReason: ArrayStack[Literal]): Unit = {
    var i = 1
    while (i < literals.length) {
      outReason.append(literals(i).opposite)
      i += 1
    }
  }
  
  final override def explainFail(outReason: ArrayStack[Literal]): Unit = {
    var i = 0
    while (i < literals.length) {
      outReason.append(literals(i).opposite)
      i += 1
    }
  }
  
  final override def setup(): Boolean = {
    
    var unit = false
    
    // Search first literals

    // Swap first 
    
    // Search second literals
    
    // Search for two literals to watch

    // If entailed return true
    
    // If asserting on literal
    // literal becomes 0
    // store.enqueue(literals(0), this)
    
    // Register the clause
    store.watch(this, literals(0))   
    store.watch(this, literals(1))
    
    if (unit) store.enqueue(literals(0), this)
    else true
  }

  final override def propagate(literal: Literal): Boolean = {
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
