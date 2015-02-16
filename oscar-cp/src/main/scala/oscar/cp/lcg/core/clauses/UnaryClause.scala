package oscar.cp.lcg.core.clauses

import oscar.cp.lcg.core.LCGStore
import oscar.algo.ArrayStack
import oscar.cp.lcg.core.Literal
import oscar.cp.lcg.core.False

class UnaryClause(store: LCGStore, literal: Literal, learnt: Boolean) extends Clause {

  final override def explainUnit(outReason: ArrayStack[Literal]): Unit = {}

  final override def explainFail(outReason: ArrayStack[Literal]): Unit = {
    outReason.append(literal.opposite)
  }

  final override def propagate(opposite: Literal): Boolean = {
    // Keep watching this literal
    store.watch(this, opposite)
    store.enqueue(literal, this)
  }
    
  final override def setup(): Boolean = {
    // Watch the signed literal
    store.watch(this, literal.opposite)
    store.enqueue(literal, this)
  }
  
  final override def toString: String = s"($literal)"
}