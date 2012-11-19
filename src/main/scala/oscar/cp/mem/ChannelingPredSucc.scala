package oscar.cp.mem

import oscar.cp.core._

/** ChannelingPredSucc
 *
 *  This constraint aims to link predecessors and successors of visits
 *  in symmetric routing problems.
 *
 *  @author Renaud Hartert - ren.hartert@gmail.com
 */

class ChannelingPredSucc(cp: Store, pred: Array[CPVarInt], succ: Array[CPVarInt]) extends Constraint(cp, "ChannelingPredSucc") {

  private val FAIL    = CPOutcome.Failure
  private val SUSPEND = CPOutcome.Suspend

  private val nSites = pred.size
  private val Sites  = 0 until nSites

  override def setup(l: CPPropagStrength): CPOutcome = {

    if (propagate() == FAIL) return FAIL

    for (s <- Sites) {

      if (!pred(s).isBound) pred(s).callValBindIdxWhenBind(this, s)
      if (!pred(s).isBound) pred(s).callValRemoveIdxWhenValueIsRemoved(this, s)
      
      // Differentiates calls from pred with +nSites
      if (!succ(s).isBound) succ(s).callValBindIdxWhenBind(this, s + nSites)
      if (!succ(s).isBound) succ(s).callValRemoveIdxWhenValueIsRemoved(this, s + nSites)
    }

    SUSPEND
  }

  override def propagate(): CPOutcome = {

    for (i <- Sites; j <- Sites) {

      // Predecessor
      if (pred(i).hasValue(j)) {
        if (pred(i).isBound) {
          if (succ(j).assign(i) == FAIL) return FAIL
        } else if (!succ(j).hasValue(i)) {
          if (pred(i).removeValue(j) == FAIL) return FAIL
        }
      }

      // Successor
      if (succ(i).hasValue(j)) {
        if (succ(i).isBound) {
          if (pred(j).assign(i) == FAIL) return FAIL
        } else if (!pred(j).hasValue(i)) {
          if (succ(i).removeValue(j) == FAIL) return FAIL
        }
      }
    }

    return SUSPEND
  }

  override def valRemoveIdx(cpvar: CPVarInt, i: Int, j: Int): CPOutcome =
    if (i < nSites) removePred(i, j)
    else removeSucc(i - nSites, j)

  override def valBindIdx(cpvar: CPVarInt, i: Int): CPOutcome =
    if (i < nSites) bindPred(i)
    else bindSucc(i - nSites)

  private def removePred(i: Int, j: Int): CPOutcome = succ(j).removeValue(i)
  private def removeSucc(i: Int, j: Int): CPOutcome = pred(j).removeValue(i)

  private def bindPred(i: Int): CPOutcome = succ(pred(i).value).assign(i)
  private def bindSucc(i: Int): CPOutcome = pred(succ(i).value).assign(i)
}

