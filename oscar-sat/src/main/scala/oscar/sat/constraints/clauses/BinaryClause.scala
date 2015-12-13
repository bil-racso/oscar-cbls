package oscar.sat.constraints.clauses

import oscar.sat.core.CDCLStore
import oscar.sat.core.True
import oscar.algo.array.ArrayStackInt

class BinaryClause(solver: CDCLStore, _lit1: Int, _lit2: Int) extends Clause {

  private[this] var lit1: Int = _lit1 // the unit literal
  private[this] var lit2: Int = _lit2

  final override def remove(): Unit = Unit

  final override def simplify(): Boolean = true
  
  final override def explain(outReason: ArrayStackInt): Unit = {
    outReason.append(lit2 ^ 1)
  }
  
  final override def explainAll(outReason: ArrayStackInt): Unit = {
    outReason.append(lit1 ^ 1)
    outReason.append(lit2 ^ 1)
  }

  final override def propagate(literal: Int): Boolean = {
    
    // Make sure the false literal is lit2
    if (lit1 == (literal ^ 1)) {
      lit1 = lit2
      lit2 = literal ^ 1
    }

    // Keep watching the literal
    solver.watch(this, literal)
    
    // Propagate
    solver.enqueue(lit1, this)
  }
  
  final override def toString: String = lit1 + " " + lit2
}