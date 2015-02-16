package oscar.cp.lcg.core.clauses

import oscar.cp.lcg.core.Literal
import oscar.cp.lcg.core.LCGStore
import oscar.cp.lcg.core.LiftedBoolean
import oscar.cp.lcg.core.Unassigned
import oscar.cp.lcg.core.False
import oscar.cp.lcg.core.True

class ClauseBuilder(lcgStore: LCGStore) {

  private[this] val trueLit: Literal = lcgStore.trueLit
  private[this] val falseLit: Literal = lcgStore.falseLit

  private[this] var array: Array[Int] = new Array(100000) // FIXME: dirty hack
  private[this] var magic: Int = 1

  private[this] var clauseBuffer: Array[Literal] = new Array(8)
  private[this] var clauseSize: Int = 0
  private[this] var status: LiftedBoolean = Unassigned

  final def clear(): Unit = {
    status = Unassigned
    clauseSize = 0
    if (magic < Int.MaxValue) magic += 1
    else {
      magic = Int.MinValue + 1
      var i = array.length
      while (i > 0) {
        i -= 1
        array(i) = Int.MinValue
      }
    }
  }

  final def add(lit: Literal): LiftedBoolean = {
    if (status != Unassigned) status
    else {
      val id = lit.id
      if (array(id) == magic) Unassigned // lit is already in the clause
      else if (lit == falseLit) Unassigned // no impact
      else if (lit == trueLit) { // the clause is entailed
        status = True
        True
      } else {
        val opId = id ^ 1
        if (array(opId) == magic) { // contradiction
          status = True
          True
        } else {
          // Add the literal to the clause
          if (clauseBuffer.length == clauseSize) growClause()
          clauseBuffer(clauseSize) = lit
          clauseSize += 1
          Unassigned
        }
      }
    }
  }

  final def toArray: Array[Literal] = {
    if (status == True || clauseSize == 0) null
    else {
      val literals = new Array[Literal](clauseSize)
      System.arraycopy(clauseBuffer, 0, literals, 0, clauseSize)
      literals
    }
  }

  @inline private def growClause(): Unit = {
    val newBuffer = new Array[Literal](clauseSize * 2)
    System.arraycopy(clauseBuffer, 0, newBuffer, 0, clauseSize)
    clauseBuffer = newBuffer
  }
}