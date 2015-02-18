package oscar.lcg.core.clause

import oscar.algo.ArrayStack
import oscar.lcg.core.CDCLStore
import oscar.lcg.core.Literal

/** @author Renaud Hartert ren.hartert@gmail.com */
class UnaryClause(store: CDCLStore, literal: Literal, learnt: Boolean) extends Clause {

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