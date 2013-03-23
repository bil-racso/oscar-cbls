package oscar.cp.mem.constraints
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPOutcome._

/** ChannelingPredSucc
 *
 *  This constraint aims to link predecessors and successors of visits
 *  in symmetric routing problems.
 *
 *  @author Renaud Hartert ren.hartert@gmail.com
 */

class ChannelingPredSucc(pred: Array[CPVarInt], succ: Array[CPVarInt]) extends Constraint(pred.head.store, "ChannelingPredSucc") {

  private val nSites = pred.size
  private val Sites  = 0 until nSites

  override def setup(l: CPPropagStrength): CPOutcome = {

    if (propagate() == Failure) return Failure

    for (s <- Sites) {

      if (!pred(s).isBound) pred(s).callValBindIdxWhenBind(this, s)
      if (!pred(s).isBound) pred(s).callValRemoveIdxWhenValueIsRemoved(this, s)
      
      // Differentiates calls from pred with +nSites
      if (!succ(s).isBound) succ(s).callValBindIdxWhenBind(this, s + nSites)
      if (!succ(s).isBound) succ(s).callValRemoveIdxWhenValueIsRemoved(this, s + nSites)
    }

    Suspend
  }

  override def propagate(): CPOutcome = {

    for (i <- Sites; j <- Sites) {

      // Predecessor
      if (pred(i).hasValue(j)) {
        if (pred(i).isBound) {
          if (succ(j).assign(i) == Failure) return Failure
        } else if (!succ(j).hasValue(i)) {
          if (pred(i).removeValue(j) == Failure) return Failure
        }
      }

      // Successor
      if (succ(i).hasValue(j)) {
        if (succ(i).isBound) {
          if (pred(j).assign(i) == Failure) return Failure
        } else if (!pred(j).hasValue(i)) {
          if (succ(i).removeValue(j) == Failure) return Failure
        }
      }
    }

    return Suspend
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

object ChannelingPredSucc {
  def apply(pred: Array[CPVarInt], succ: Array[CPVarInt]) = new ChannelingPredSucc(pred, succ)
}

