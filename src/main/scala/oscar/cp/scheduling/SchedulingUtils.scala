package oscar.cp.scheduling

import scala.util.continuations._
import oscar.cp.core.CPVarInt
import oscar.reversible.ReversibleBool
import oscar.reversible.ReversibleInt

object SchedulingUtils {

  /** Returns the id of the task with minimal EST (minimal ECT as tie breaker). */
  private def minTask(tasks: Iterable[Int], starts: IndexedSeq[CPVarInt], ends: IndexedSeq[CPVarInt]): Int = {
    var minTask = -1
    var minEST = Int.MaxValue
    var minECT = Int.MaxValue
    for (t <- tasks) {
      if (starts(t).min < minEST) {
        minTask = t
        minEST = starts(t).min
        minECT = ends(t).min
      } else if (starts(t).min == minEST && ends(t).min < minECT) {
        minTask = t
        minEST = starts(t).min
        minECT = ends(t).min
      }
    }
    minTask
  }

  def setTimes(starts: Array[CPVarInt], durations: Array[CPVarInt], ends: Array[CPVarInt]): Unit @suspendable = {

    val cp = starts.head.store
    val nTasks = starts.size
    val Tasks = 0 until nTasks

    // Tasks to schedule
    val selectable = Array.fill(nTasks)(new ReversibleBool(cp, true))  
    // Scheduled tasks
    val scheduled = Array.fill(nTasks)(new ReversibleBool(cp, false))
    // Value of the ESTs when tasks became postponed
    val oldEST = Array.fill(nTasks)(new ReversibleInt(cp, -1))
    
    // Returns true if the task is bound
    def isBound(t: Int): Boolean = starts(t).isBound && ends(t).isBound
    // Returns true if all tasks are bound
    def allBound: Boolean = Tasks.forall(isBound(_))
    // Updates the status of the tasks
    def updateSelectable() = {
      for (t <- Tasks) {
        if (isBound(t)) selectable(t).value = false
        else if (oldEST(t).value < starts(t).min) selectable(t).value = true
      }
    }
    // Returns the indices of the selectable tasks
    def selectableIndices() = (Tasks).filter(i => selectable(i).value)

    def checkDominance(t: Int): Boolean = {
      for (j <- Tasks; if !selectable(j).value && !scheduled(j).value && ends(j).min <= starts(t).min && starts(j).max <= starts(t).min) {
        return true
      }
      return false
    }

    def updateAndCheck() = {
      updateSelectable()
      if (selectableIndices().isEmpty && !allBound) cp.fail()
    }

    updateSelectable()

    while (!allBound) {

      val x = minTask(selectableIndices(), starts, ends)

      if (checkDominance(x)) cp.fail()

      cp.branch {
        if (!cp.isFailed) {
          cp.assign(starts(x), starts(x).min)
          scheduled(x).value = true
          if (!cp.isFailed) updateAndCheck()
        }
      } {
        if (!cp.isFailed) {
          selectable(x).value = false
          oldEST(x).value = starts(x).min
          updateAndCheck()
        }
      }
    }
  }
}