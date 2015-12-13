package oscar.sat.constraints.nogoods

import oscar.sat.core.CDCLStore
import oscar.algo.array.ArrayStackInt

final class BinaryNogood(solver: CDCLStore, _lit1: Int, _lit2: Int) extends Nogood {

  override var activity: Double = 0
  
  override def locked: Boolean = solver.assignReason(lit1 / 2) == this

  private[this] var lit1: Int = _lit1 // the unit literal
  private[this] var lit2: Int = _lit2

  override def remove(): Unit = Unit

  override def simplify(): Boolean = true
  
  override def explain(outReason: ArrayStackInt): Unit = {
    outReason.append(lit2 ^ 1)
  }
  
  final override def explainAll(outReason: ArrayStackInt): Unit = {
    outReason.append(lit1 ^ 1)
    outReason.append(lit2 ^ 1)
  }

  override def propagate(literal: Int): Boolean = {
    
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
  
  override def toString: String = lit1 + " " + lit2
}