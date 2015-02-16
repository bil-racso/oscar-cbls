package oscar.lcg.core.clause

import oscar.algo.ArrayStack
import oscar.lcg.core.CDCLStore
import oscar.lcg.core.Literal
import oscar.lcg.core.True
import oscar.lcg.core.False

class NaryClause(store: CDCLStore, literals: Array[Literal], learnt: Boolean) extends Clause {
  
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
    
    var unassigned = 0
    val nLiterals = literals.length
    
    // Search a first literals to watch
    var i = 0
    var litId = -1
    var maxLevel = -1
    while (i < nLiterals) {
      val lit = literals(i)
      val level = store.assignedLevel(lit)
      if (level == -1) {
        unassigned += 1
        litId = i
        i = nLiterals
      } else if (level > maxLevel) {
        maxLevel = level
        litId = i
      }
      i += 1
    }

    // Swap first 
    val tmp1 = literals(0)
    literals(0) = literals(litId)
    literals(litId) = tmp1
    
    // Search a second literals to watch
    i = 1
    litId = -1
    maxLevel = -1
    while (i < nLiterals) {
      val lit = literals(i)
      val level = store.assignedLevel(lit)
      if (level == -1) {
        unassigned += 1
        litId = i
        i = nLiterals
      } else if (level > maxLevel) {
        maxLevel = level
        litId = i
      }
      i += 1
    }
    
    // Swap second
    val tmp2 = literals(1)
    literals(1) = literals(litId)
    literals(litId) = tmp2
    
    // Register the clause
    store.watch(this, literals(0).opposite)   
    store.watch(this, literals(1).opposite)
    
    // Propagate if unit
    if (unassigned > 1) true
    else store.enqueue(literals(0), this)
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
