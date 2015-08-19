package oscar.lcg.clauses

import oscar.lcg.core.Clause
import oscar.lcg.core.Literal
import oscar.lcg.literals.LitFalse

class WLClause(literals: Array[Literal], learnt: Boolean = false) extends Clause {

  assert(literals.length > 1)

  private[this] val nLiterals = literals.length
  private[this] val watchLit1 = nLiterals - 1
  private[this] val watchLit2 = nLiterals - 2

  override def setup(): Boolean = {
    if (learnt) initDeepest()
    else if (!initFirstUnassigned()) checkValidity()
    else if (!initSecondUnassigned()) initDeepest()
    val lit1 = literals(watchLit1)
    val lit2 = literals(watchLit2)
    lit1.opposite.watch(this)
    lit2.opposite.watch(this)
    // Propagate if all literals are false
    if (!lit2.isFalse) true
    else {
      lit1.explain(literals, nLiterals - 1)
      lit1.assign()
    }
  }

  override def propagate(oppLit: Literal): Boolean = {

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
    val array = Array.tabulate(nLiterals - 1)(i => literals(i).opposite)
    lit1.explain(array, nLiterals - 1)
    lit1.assign()
  }

  @inline private def initFirstUnassigned(): Boolean = {
    var i = 0
    while (i < nLiterals && literals(i).isAssigned) i += 1
    if (i == nLiterals) false
    else {
      assert(!literals(i).isAssigned)
      val tmp = literals(i)
      literals(i) = literals(watchLit1)
      literals(watchLit1) = tmp
      true
    }
  }

  @inline private def initSecondUnassigned(): Boolean = {
    var i = 0
    while (i < watchLit1 && literals(i).isAssigned) i += 1
    if (i == watchLit1) false
    else {
      assert(!literals(i).isAssigned)
      val tmp = literals(i)
      literals(i) = literals(watchLit2)
      literals(watchLit2) = tmp
      true
    }
  }
  
  @inline private def checkValidity(): Boolean = {
    var i = nLiterals
    while (i > 0) { i -= 1; if (literals(i).isTrue) return true }
    val array = Array.tabulate(nLiterals)(i => literals(i).opposite)
    LitFalse.explain(array)
    false
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
    val tmp = literals(maxId)
    literals(maxId) = literals(watchLit2)
    literals(watchLit2) = tmp
  }
}