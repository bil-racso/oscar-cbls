package oscar.sat.constraints

import oscar.algo.array.ArrayStackInt
import oscar.sat.core.CDCLStore
import oscar.sat.core.False
import oscar.sat.core.True

final class NaryClause(literals: Array[Int], store: CDCLStore) extends Clause {
  
  private[this] var _deleted = false
  
  final override def deleted: Boolean = !_deleted
  
  final override def delete(): Unit = _deleted = true
  
  final override def explainAssign(reason: ArrayStackInt): Unit = {
    var i = literals.length
    while (i > 1) {
      i -= 1
      reason.append(literals(i) ^ 1)
    }
  }
  
  final override def explainFail(reason: ArrayStackInt): Unit = {
    var i = literals.length
    while (i > 0) {
      i -= 1
      reason.append(literals(i) ^ 1)
    }
  }
  
  /** Propagate litId set to true. */
  final override def propagate(satisfiedLit: Int): Boolean = {
    
    // Make sure the false literal is literals(1)
    val falsifiedLit = satisfiedLit ^ 1
    if (literals(0) == falsifiedLit) {
      literals(0) = literals(1)
      literals(1) = falsifiedLit
    }
    
    // If 0th literal is true, then the clause is already statisfied
    if (store.litValue(literals(0)) == True) {
      store.watch(this, satisfiedLit)
      return true
    }
    
    var i = 2
    val nLiterals = literals.length
    while (i < nLiterals) {
      val litId = literals(i)
      if (store.litValue(litId) != False) {
        literals(1) = litId
        literals(i) = falsifiedLit
        store.watch(this, litId ^ 1)
        return true
      }
      i += 1
    }
    
    // Unit propagation
    store.watch(this, satisfiedLit)
    store.enqueue(literals(1), this)
  }
}