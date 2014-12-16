package oscar.cp.linearizedDFS.branching

import oscar.cp.linearizedDFS.{SetTimesRightBranch, SetTimesLeftBranch}
import oscar.algo.reversible.{ReversibleInt, ReversibleBoolean}
import oscar.algo.search.Branching
import oscar.cp.core.CPIntVar
import oscar.cp.modeling._

/**
 * Created by saschavancauwelaert on 09/12/14.
 */

class SetTimesBranchingToReplay(starts: IndexedSeq[CPIntVar], durations: IndexedSeq[CPIntVar], ends: IndexedSeq[CPIntVar], tieBreaker: Int => Int = (i: Int) => i) extends Branching {

  val cp = starts.head.store
  val n = starts.size
  val Activities = 0 until n

  val selectable = Array.fill(n)(new ReversibleBoolean(cp, true))
  // non fixed activities (by setTimes)
  val bound = Array.fill(n)(new ReversibleBoolean(cp, false))

  val oldEST = Array.fill(n)(new ReversibleInt(cp, -1))

  // update the new ones becoming available because est has moved
  def updateSelectable() = (Activities).filter(i => oldEST(i).value < starts(i).min || durations(i).max == 0).foreach(selectable(_).value = true)
  def selectableIndices() = (Activities).filter(i => selectable(i).value && !bound(i).value)
  def allStartBounds() = bound.forall(i => i.value)

  def updateAndCheck() = {
    updateSelectable()
    if (selectableIndices().isEmpty && !allStartBounds()) cp.fail()
  }

  def alternatives(): Seq[Alternative] = {
    if (allStartBounds()
      || starts.forall(_.isBound))
    {
      noAlternative
    } else {
      updateSelectable()
      val (est, ect,x) = selectableIndices().map(i => (starts(i).min, tieBreaker(i),i)).min
      // Select the activity with the smallest EST, ECT as tie breaker
      Seq(SetTimesLeftBranch(starts, est,x, bound, updateAndCheck, true),
      SetTimesRightBranch(starts(x).store, est, x, selectable, oldEST, updateAndCheck, true))

    }

  }
}