package oscar.cp.scheduling

import scala.util.continuations._
import oscar.cp.core.CPVarInt
import oscar.algo.reversible.ReversibleBool
import oscar.algo.reversible.ReversibleInt

object SchedulingUtils {

  @deprecated(message = "Use search/start instead instead of non-deterministic search, @see SetTimesBranching in particular", since = "1.0")
  def setTimes(starts: Array[CPVarInt], durations: Array[CPVarInt], ends: Array[CPVarInt]): Unit @suspendable = {

    val cp = starts.head.store
    val n = starts.size
    val Activities = 0 until n

    val selectable = Array.fill(n)(new ReversibleBool(cp, true))
    // non fixed activities (by setTimes)
    val bound = Array.fill(n)(new ReversibleBool(cp, false))

    val oldEST = Array.fill(n)(new ReversibleInt(cp, -1))

    // update the new ones becoming available because est has moved
    def updateSelectable() = (Activities).filter(i => oldEST(i).value < starts(i).min || durations(i).max == 0).foreach(selectable(_).value = true)
    def selectableIndices() = (Activities).filter(i => selectable(i).value && !bound(i).value)
    def allStartBounds() = bound.forall(i => i.value)

    def updateAndCheck() = {
      updateSelectable()
      if (selectableIndices().isEmpty && !allStartBounds()) cp.fail()
    }

    while (!allStartBounds()) {

      updateSelectable()
      val (est, ect) = selectableIndices().map(i => (starts(i).min, ends(i).min)).min

      // Select the activity with the smallest EST, ECT as tie breaker
      val x = selectableIndices().filter(i => starts(i).min == est && ends(i).min == ect).head

      cp.branch {
        cp.post(starts(x) == est)
        bound(x).value = true
        if (!cp.isFailed) updateAndCheck()
      } {
        selectable(x).value = false
        oldEST(x).value = est
        updateAndCheck()
      }
    }
  }
}
