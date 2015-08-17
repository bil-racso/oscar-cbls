package oscar.lcg.clauses

import oscar.lcg.core.Clause
import oscar.lcg.core.Literal

class WLClause(literals: Array[Literal], initUnassigned: Boolean = true) extends Clause {

  assert(literals.length > 1)
  
  private[this] val nLiterals = literals.length
  private[this] val watchLit1 = nLiterals - 1
  private[this] val watchLit2 = nLiterals - 2
  
  override def setup(): Boolean = {
    if (initUnassigned) initUnassigned()
    initDeepest()
    literals(watchLit1).opposite.watch(this)
    literals(watchLit2).opposite.watch(this)
    // Propagate if all literals are false
    if (!literals(watchLit2).isFalse) true
    else literals(watchLit1).assign(literals, nLiterals - 1)
  }
  
  override def propagate(oppLit: Literal): Boolean = {
    
    println("propagate " + literals.mkString(" "))
    
    // Falsified literal
    val lit2 = oppLit.opposite
    
    // Make sure the flase literal is watchLit2
    if (literals(watchLit1) == lit2) {
      literals(watchLit1) = literals(watchLit2)
      literals(watchLit2) = lit2
    }
    
    // If watchLit1 is true, the clause is entailed
    val lit1 = literals(watchLit1)
    if (lit1.isTrue) {
      oppLit.watch(this)
      return true
    }

    // Look for a new literal to watch
    var i = watchLit2
    while (i > 0) {
      i -= 1
      val lit = literals(i)
      if (!lit.isFalse) {
        literals(watchLit2) = lit
        literals(i) = lit2
        lit.opposite.watch(this)
        return true
      }
    }
    
    // Unit propagation
    oppLit.watch(this)
    lit1.assign(literals, nLiterals - 1)
  }
  
  @inline private def initUnassigned(): Unit = {
    var i = 0
    while (i < nLiterals && literals(i).isAssigned) i += 1
    assert(!literals(i).isAssigned)
    val tmp = literals(i)
    literals(i) = literals(watchLit1) 
    literals(watchLit1) = tmp
  }
  
  @inline private def initDeepest(): Unit = {
    var i = watchLit1 // dicard the unassigned literal
    var maxId = -1
    var maxLevel = Int.MinValue
    while (i > 0) {
      i -= 1
      val lit = literals(i)
      val level = lit.level
      if (level > maxLevel) {
        maxLevel = level 
        maxId = i
      }
    }
    assert(maxId != -1)
    //assert(literals(maxId).isAssigned)
    val tmp = literals(maxId)
    literals(maxId) = literals(watchLit2) 
    literals(watchLit2) = tmp
  }
}