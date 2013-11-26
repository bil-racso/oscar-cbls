package oscar.cp.constraints

import scala.collection.mutable.HashSet
import scala.collection.mutable.TreeSet
import scala.math._
import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.core.CPOutcome._
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPStore
import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.modeling._
import oscar.cp.modeling.CPScheduler
import oscar.visual.VisualUtil
import oscar.visual.scheduling.VisualGanttChart
import oscar.cp.scheduling.SchedulingUtils
import oscar.visual.VisualFrame
import scala.io.Source

class EnergeticReasoning(starts: Array[CPVarInt], ends: Array[CPVarInt], durations: Array[CPVarInt], demands: Array[CPVarInt], resources: Array[CPVarInt], capacity: CPVarInt, id: Int = 1) extends Constraint(starts.head.store, "Energetic Reasoning") {

  assert(starts.length == durations.length && starts.length == ends.length && starts.length == demands.length && starts.length == resources.length, "starts, durations, ends, demands and resources must be of same length")

  def setup(l: CPPropagStrength): CPOutcome = {
    if (propagate == Failure) Failure
    else {
      for (task <- 0 until starts.length) {
        starts(task).callPropagateWhenBoundsChange(this)
        durations(task).callPropagateWhenBoundsChange(this)
        ends(task).callPropagateWhenBoundsChange(this)
        demands(task).callPropagateWhenBoundsChange(this)
        resources(task).callPropagateWhenBind(this)
      }
      capacity.callPropagateWhenBoundsChange(this)
      Suspend
    }
  }

  override def propagate: CPOutcome = {

    //keep only the tasks that we know are assigned to the resource id considered by this constraint 
    val tasks = (0 until starts.length) filter (task => resources(task).isBound && resources(task).getValue == id && durations(task).min > 0 && demands(task).min > 0)

    val newEets = ends map (_.min)
    val newLsts = starts map (_.max)

    val intervals = computeIntervals(tasks)

    for ((t1, t2) <- intervals) {
      val currentIntervalEnergy = energyForInterval(t1, t2, tasks)
      val currentMaxIntervalEnergy = capacity.min * (t2 - t1)
      if (currentIntervalEnergy > currentMaxIntervalEnergy) {
        if (capacity.updateMin(ceil(currentIntervalEnergy.toDouble / (t2 - t1)).toInt) == Failure) {
          return Failure
        } else
          return Suspend
      } else {
        //bound adjustements computation
        for (task <- tasks) {
          val slackWithoutCurrentActivity = currentMaxIntervalEnergy - currentIntervalEnergy + activityEnergyForInterval(task, t1, t2, tasks)
          val leftShiftedEnergy = leftShiftedActivityEnergyForInterval(task, t1, t2, tasks)
          val rightShiftedEnergy = rightShiftedActivityEnergyForInterval(task, t1, t2, tasks)

          if (slackWithoutCurrentActivity < leftShiftedEnergy)
            newEets(task) = max(newEets(task), t2 + ceil((leftShiftedEnergy - slackWithoutCurrentActivity).toDouble / demands(task).min).toInt)

          if (slackWithoutCurrentActivity < rightShiftedEnergy)
            newLsts(task) = min(newLsts(task), t1 - ceil((rightShiftedEnergy - slackWithoutCurrentActivity).toDouble / demands(task).min).toInt)

        }
      }
    }

    //apply bound adjustements
    for (task <- tasks) {
      if (starts(task).updateMax(newLsts(task)) == Failure) {
        return Failure
      }

      if (ends(task).updateMin(newEets(task)) == Failure) {
        return Failure
      }

    }

    Suspend
  }

  @inline
  private def computeIntervals(tasks: IndexedSeq[Int]) = {
    val (o1, o2, ot) = getO1_O_2_Ot(tasks)
    val intervals = HashSet[Tuple2[Int, Int]]()

    for (t1 <- o1; t2 <- o2 if t1 < t2)
      intervals += Tuple2(t1, t2)

    for (o <- ot) {
      for (s <- o1 if (o(s) >= 0 && s < o(s)))
        intervals += Tuple2(s, o(s))

      for (e <- o2 if (o(e) >= 0 && e > o(e)))
        intervals += Tuple2(o(e), e)
    }
    intervals
  }

  @inline
  private def getO1_O_2_Ot(tasks: IndexedSeq[Int]) = {

    val o1 = TreeSet[Int]()
    val o2 = TreeSet[Int]()
    val ot = HashSet[(Int) => Int]()

    for (task <- tasks) {
      o1 += starts(task).getMin //est
      o1 += ends(task).getMin //ect
      o1 += starts(task).getMax //lst

      o2 += starts(task).getMax //lst
      o2 += ends(task).getMin //ect
      o2 += ends(task).getMax //lct

      ot += ((t: Int) => starts(task).getMin + ends(task).getMax - t) //est + lct - t
    }

    (o1, o2, ot)
  }

  @inline
  private def energyForInterval(t1: Int, t2: Int, tasks: IndexedSeq[Int]) = {
    var energy = 0

    for (task <- tasks)
      energy += activityEnergyForInterval(task, t1, t2, tasks)

    energy
  }

  @inline
  private def leftShiftedActivityEnergyForInterval(task: Int, t1: Int, t2: Int, tasks: IndexedSeq[Int]) = min(t2 - t1, min(durations(task).min, max(0, ends(task).min - t1))) * demands(task).min

  @inline
  private def rightShiftedActivityEnergyForInterval(task: Int, t1: Int, t2: Int, tasks: IndexedSeq[Int]) = min(t2 - t1, min(durations(task).min, max(0, t2 - starts(task).max))) * demands(task).min

  @inline
  private def activityEnergyForInterval(task: Int, t1: Int, t2: Int, tasks: IndexedSeq[Int]) = min(leftShiftedActivityEnergyForInterval(task, t1, t2, tasks), rightShiftedActivityEnergyForInterval(task, t1, t2, tasks))

}

