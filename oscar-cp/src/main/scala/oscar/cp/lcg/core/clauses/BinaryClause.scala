package oscar.cp.lcg.core.clauses

import oscar.cp.lcg.core.LCGStore
import oscar.cp.lcg.core.Literal
import oscar.cp.lcg.core.True
import oscar.cp.lcg.core.False
import oscar.cp.lcg.core.Unassigned
import oscar.algo.ArrayStack

class BinaryClause(store: LCGStore, firstLiteral: Literal, secondLiteral: Literal, learnt: Boolean) extends Clause {
  
  private[this] var firstAssigned = false

  final override def explainUnit(outReason: ArrayStack[Literal]): Unit = {
    if (firstAssigned) outReason.append(secondLiteral.opposite)
    else outReason.append(firstLiteral.opposite)
  }

  final override def explainFail(outReason: ArrayStack[Literal]): Unit = {
    outReason.append(firstLiteral.opposite)
    outReason.append(secondLiteral.opposite)
  }

  final override def propagate(literal: Literal): Boolean = {
    // Keep watching this literal
    store.watch(this, literal)
    // Assert the other literal
    firstAssigned = literal.opposite == firstLiteral
    if (firstAssigned) store.enqueue(secondLiteral, this)
    else store.enqueue(firstLiteral, this)
  }
    
  final override def setup(): Boolean = {
    // Watch both literal
    store.watch(this, firstLiteral.opposite)
    store.watch(this, secondLiteral.opposite)
    // Check for initial assertion
    val firstValue = store.value(firstLiteral)   
    if (firstValue == Unassigned) {
      val secondValue = store.value(secondLiteral)
      if (secondValue == Unassigned) true
      else if (secondValue == False) {
        firstAssigned = true
        store.enqueue(firstLiteral, this)
      }
      else {
        firstAssigned = false
        true
      }
    }
    else if (firstValue == True) {
      firstAssigned = true
      true
    }
    else {
      firstAssigned = false
      store.enqueue(secondLiteral, this)
    }
  }
  
  final override def toString: String = s"($firstLiteral, $secondLiteral)"
}