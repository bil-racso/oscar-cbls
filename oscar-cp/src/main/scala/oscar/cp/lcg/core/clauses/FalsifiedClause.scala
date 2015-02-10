package oscar.cp.lcg.core.clauses

import oscar.cp.lcg.core.Literal
import oscar.algo.ArrayStack

class FalsifiedClause(literals: Array[Literal]) extends Clause {

  /** Propagate the clause with no assumption. */
  override def setup(): Boolean = ???
  
  /** Propagate the clause when !literal becomes false. */
  override def propagate(literal: Literal): Boolean = ???
  
  /** Explains the last assertion. */
  override def explainUnit(outReason: ArrayStack[Literal]): Unit = ???
  
  /** Explains the last fail. */
  override def explainFail(outReason: ArrayStack[Literal]): Unit = {
    var i = literals.length
    while (i > 0) { i -= 1; outReason.append(literals(i)) }
  }
}