/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */
package oscar.cbls.scheduling.model

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *            Yoann Guyot
 * ****************************************************************************
 */

import oscar.cbls.invariants.core.computation.{ CBLSSetVar, CBLSIntVar, Store }
import oscar.cbls.invariants.lib.minmax.{ ArgMinArray, ArgMaxArray }
import oscar.cbls.invariants.lib.logic.{ Filter, DenseRef }
import oscar.visual.VisualFrame
import oscar.visual.plot.PlotLine
import oscar.cbls.scheduling.visu.Gantt
import oscar.cbls.modeling.Algebra._

/**
 * @param model
 * @param maxduration
 * @author renaud.delandtsheer@cetic.be
 */
class Planning(val model: Store, val maxDuration: Int) {

  var isClosed = false

  var resources: List[Resource] = List.empty
  var resourceCount: Int = 0
  /**called by resources registers it in the planning, returns an ID, which is the one of the resource*/
  def addResource(r: Resource): Int = {
    resources = r :: resources
    resourceCount += 1
    resourceCount - 1
  }

  var superActivity = false
  var activities: List[Activity] = List.empty
  var activityCount: Int = 0
  /**called by activities registers it in the planning, returns an ID, which is the one of the activity*/
  def addActivity(j: Activity): Int = {
    activities = j :: activities
    activityCount += 1
    activityCount - 1
  }

  var earliestStartDates: Array[CBLSIntVar] = null
  var earliestEndDates: Array[CBLSIntVar] = null
  var latestStartDates: Array[CBLSIntVar] = null

  val makeSpan: CBLSIntVar = CBLSIntVar(model, 0, maxDuration, 0, "makeSpan")
  var earliestOvershotResources: CBLSSetVar = null
  var worseOvershotResource: CBLSSetVar = null

  var resourceArray: Array[Resource] = null
  var activityArray: Array[Activity] = null

  var sentinelActivity: Activity = null //a task that is added after all activities, to simplify algorithm construction

  model.addToCallBeforeClose(_ => this.close())

  /**
   * this is to close the planning when you are done with declaring tasks, precedence  and resource
   * notice that you do not need to explicitely call this, as the model will call it automatically on close.
   */
  def close() {
    if (isClosed) return
    isClosed = true
    val activitiesNoSentinel = activities
    sentinelActivity = new Activity(0, this, "sentinelActivity")
    sentinelActivity.latestEndDate := maxDuration

    for (a <- activitiesNoSentinel) {
      sentinelActivity.addStaticPredecessor(a)
    }

    activityArray = new Array[Activity](activityCount)
    earliestEndDates = new Array[CBLSIntVar](activityCount)
    earliestStartDates = new Array[CBLSIntVar](activityCount)
    latestStartDates = new Array[CBLSIntVar](activityCount)

    for (j <- activities) {
      activityArray(j.ID) = j
      earliestStartDates(j.ID) = j.earliestStartDate
      earliestEndDates(j.ID) = j.earliestEndDate
      latestStartDates(j.ID) = j.latestStartDate
      if (j.isInstanceOf[SuperActivity]) superActivity = true
    }

    for (j <- activities) { j.close() }

    DenseRef(activityArray.map(job => job.allPrecedingActivities), activityArray.map(job => job.allSucceedingActivities))

    makeSpan <== sentinelActivity.earliestStartDate

    resourceArray = new Array[Resource](resourceCount)
    for (r <- resources) {
      resourceArray(r.ResourceID) = r; r.close()
    }

    val WorseOvershootArray: Array[CBLSIntVar] = new Array[CBLSIntVar](resourceCount)
    for (r <- resources) {
      WorseOvershootArray(r.ResourceID) = r.overShoot
    }

    val ResourceWithOvershoot: CBLSSetVar = Filter(WorseOvershootArray)

    worseOvershotResource = ArgMaxArray(WorseOvershootArray, ResourceWithOvershoot)
  }

  var gantt: Gantt = null
  var plot: PlotLine = null
  def displayVisualRendering() {
    val frame = new VisualFrame("Cumulative JobShop Problem", 1, 1)
    frame.setBounds(0, 0, 500, 800)
    gantt = new Gantt(this)
    frame.createFrame("Gantt chart").add(gantt)
    //   plot = new Plot2D("makespan", "iterations", "makespan");
    //   frame.createFrame("progress").add(plot)

    frame.pack
    frame.setSize(1500, 500)
  }

  def updateVisual() {
    if (gantt != null) gantt.update(1.0f, 30)
  }

  override def toString: String = toAsciiArt

  def toAsciiArt: String = {
    def nStrings(N: Int, C: String): String = (if (N <= 0) "" else "" + C + nStrings(N - 1, C))
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
    val activityList =
      activities.filter(_ != sentinelActivity).sortWith((a, b) =>
        a.earliestStartDate.value < b.earliestStartDate.value)
    val activityStrings = activityList.map(_.toAsciiArt)

    activityStrings.mkString + makeSpan + "\n"
  }

  def dependencies: String = {
    var toreturn: String = ""
    for (activity <- activities.sortBy(t => t.earliestStartDate.value)) {
      for (t2 <- activity.allSucceedingActivities.value if t2 != activity.ID && t2 != sentinelActivity.ID) {
        val activity2 = activityArray(t2)
        if (activity2.additionalPredecessors.value.contains(activity.ID)) {
          toreturn += activity.name + " -> " + activity2.name + "\n"
        } else {
          toreturn += activity.name + " ->> " + activity2.name + "\n"
        }
      }
    }
    toreturn
  }

  def resourceUsage: String = {
    resources.map(_.toAsciiArt(20)).mkString
  }
  /**
   * Checks that a dependence from --> to can be added to the graph,
   * assuming that there is a resource conflict involving them
   * @param from
   * @param to
   * @return true if a dependence can be add, false otherwise.
   */
  def canAddPrecedenceAssumingResourceConflict(from: Activity, to: Activity): Boolean = {
    //this is not straightforward since there can be some SuperTasks.
    (from != to) && to.canAddPrecedence && ((!superActivity) || !isThereDependency(to, from))
  }

  /**
   * Checks if there is a path leading from one activity to another one
   * @param from
   * @param to
   * @return true if there is a path from 'from' to 'to', false otherwise
   */
  def isThereDependency(from: Activity, to: Activity): Boolean = {
    val target = to.getEndActivity

    var Reached: List[Activity] = List.empty

    /**PRE: from is a ground activity. */
    def Search(from: Activity): Boolean = {
      if (from == target) return true
      if (from.earliestEndDate.value > target.earliestStartDate.value) {
        return false
      }

      if (from.mark) { return false }
      from.mark = true
      Reached = from :: Reached
      for (next <- from.getStartActivity.allSucceedingActivities.value) {
        val activity: Activity = activityArray(next)
        if (Search(activity)) return true
      }
      false
    }

    val toreturn = Search(from.getStartActivity)
    for (activity <- Reached) activity.mark = false
    toreturn
  }

  /**
   * returns a list of pair of activity; precedences to kill
   * to add the new dependency newFrom -> newTo
   * without introducing a cycle involving newFrom -> newTo
   *
   * it computes a cut in the dag newTo -> newFrom involving only additional dependencies
   * @param newFrom
   * @param newTo
   * @return
   */
  def getDependencyToKillToAvoidCycle(newFrom: Activity, newTo: Activity): PrecedenceCleaner = {
    val from = newTo
    val to = newFrom
    if (from == to) return HardPrecedence()

    var markedActivities: List[Activity] = List.empty
    var dependenciesToKill: List[(Activity, Activity)] = List.empty
    /**
     * marks all activities on the path linking From to To
     * all market activities are also added to MarkedActivities
     * return true if a path exist
     */
    def markPathes(from: Activity, to: Activity): Boolean = {
      if (from.mark) return true
      //TODO: we might explore the same node several times if it does not lead to to
      if (from == to) {
        if (!from.mark) {
          from.mark = true
          markedActivities = from :: markedActivities
        }
        return true
      }
      if (from.earliestEndDate.value > to.earliestStartDate.value) {
        return false
      }

      for (next <- from.getStartActivity.allSucceedingActivities.value) {
        val nextActivity: Activity = activityArray(next)
        if (markPathes(nextActivity, to)) from.mark = true
      }
      if (from.mark) {
        markedActivities = from :: markedActivities
      }
      from.mark
    }

    /**returns false if hard rock dependency, true if can be killed*/
    def findDependenciesToKill(from: Activity, to: Activity): Boolean = {
      if (from == to) return false
      if (!to.mark) { return true }
      for (prev <- to.getStartActivity.additionalPredecessors.value) {
        val prevActivity: Activity = activityArray(prev)
        if (prevActivity.mark) {
          dependenciesToKill = (prevActivity, to) :: dependenciesToKill
          prevActivity.mark = false
        }
      }
      for (prevActivity <- to.getStartActivity.staticPredecessors) {
        if (prevActivity.mark) {
          if (findDependenciesToKill(from, prevActivity)) {
            prevActivity.mark = false
          } else {
            return false
          }
        }
      }
      true
    }

    markPathes(from.getStartActivity, to.getEndActivity)
    if (!findDependenciesToKill(from.getStartActivity, to.getEndActivity)) {
      for (t <- markedActivities) t.mark = false
      HardPrecedence()
    } else {
      for (t <- markedActivities) t.mark = false
      PrecedencesCanBeKilled(dependenciesToKill)
    }
  }

  /**
   * removes all additional Activity precedences that are not tight
   */
  def clean() {
    for (t: Activity <- activityArray) {
      t.removeNonTightAdditionalPredecessors()
    }
  }
}

abstract class PrecedenceCleaner(val canBeKilled: Boolean) {
  def killDependencies(Verbose: Boolean = false) { throw new Exception("cannot kill dependencies") }
}
case class HardPrecedence() extends PrecedenceCleaner(false)

case class PrecedencesCanBeKilled(val d: List[(Activity, Activity)]) extends PrecedenceCleaner(true) {
  override def killDependencies(Verbose: Boolean = false) {
    for ((a, b) <- d) {
      b.removeDynamicPredecessor(a, Verbose)
    }
  }
  def restoreDependencies() {
    for ((a, b) <- d) {
      b.addDynamicPredecessor(a)
    }
  }
}

/**
 * This trait lets one post an earliest start date for a given task.
 *
 * @author yoann.guyot@cetic.be
 */
trait EarliestStartDate extends Planning {
  /**
   * Technically, it creates a task which ends at the wanted earliest start date,
   * and posts it as a static predecessor of the given task.
   *
   * Note: It seems that using an Activity (instead of a NonMoveableActivity)
   * saves scheduling engine performances.
   */
  def addEarliestStartDate(task: Activity, earliestStartDate: Int,
                           nameOfOwner: String = "") {
    if (earliestStartDate > 0) {
      val esdTaskName = "ESD_task_of_" + nameOfOwner
      val esdTask = new Activity(earliestStartDate, this, esdTaskName)
      task.addStaticPredecessor(esdTask)
    }
  }
}

/**
 * This trait lets one post variable resources.
 * Typically: resources which vary with week days.
 *
 * Technically, the trait contains a function which takes
 * an array indexing the amount of resources to the scheduler's unit,
 * modulo the array size.
 */
trait VariableResources extends Planning {
  private val resourceReductionTasks = new Array[NonMoveableActivity](maxDuration + 1)

  def postVariableResource(availabilities: Array[Int], name: String = null): CumulativeResource = {
    val nbAvailabilities = availabilities.size
    val maxAvailability = availabilities.max
    val resource = new CumulativeResource(this, maxAvailability, name)

    val mergedReductions = mergePeriods(availabilities.map(maxAvailability - _))

    for (time <- 0 to maxDuration) {
      val timeModulo = time % nbAvailabilities
      if (!mergedReductions(timeModulo).isEmpty) {
        mergedReductions(timeModulo).foreach(reduction => {
          val (duration, occupation) = reduction
          if (resourceReductionTasks(time) == null) {
            if (time + duration > maxDuration) {
              resourceReductionTasks(time) =
                new NonMoveableActivity(time, maxDuration - time, this, "ResReducAt" + time)
              println("ResReducAt" + time + " with duration " + (maxDuration - time) + " created.")
            } else {
              resourceReductionTasks(time) =
                new NonMoveableActivity(time, duration, this, "ResReducAt" + time)
              println("ResReducAt" + time + " with duration " + duration + " created.")
            }
          }
          resourceReductionTasks(time).usesCumulativeResource(resource, occupation)
          println("At " + time + ": " + occupation + " / " + resource.MaxAmount + " of " + resource.name)
        })
      }
    }

    resource
  }

  /**
   * This function produces a list of availability task specifications,
   * in the form of an array: merged(startDate) = [(task1_duration, task1_amount), ...]
   * where availabilities are "merged" when equal in several positions
   * of the given availabilities array.
   */
  private def mergePeriods(availabilities: Array[Int]): Array[List[(Int, Int)]] = {
    val modulo = availabilities.size
    var merged: Array[List[(Int, Int)]] = Array.fill(modulo)(List())
    var merging = new Array[(Int, Int)](modulo)

    /**
     * Extends the merging period starting at given time,
     * i.e. increments period's duration.
     */
    def extendPeriod(startDate: Int) {
      val period = merging(startDate)
      merging(startDate) = (period._1 + 1, period._2)
    }

    /**
     * Closes the period starting at given time, i.e.:
     * - saves the merging period in merged periods
     * - removes it from merging periods
     * - decrementing remaining amount difference by the removed period amount
     */
    def closePeriod(startDate: Int) = {
      val period = merging(startDate)
      merged(startDate) = period :: merged(startDate)
      merging(startDate) = null
      period
    }

    var previousAvailability = 0
    /**
     * For each time, we evaluate the variation of availability between
     * the current time and the previous one.
     */
    for (time <- 0 to modulo - 1) {
      val availability = availabilities(time)

      /**
       * If the availability is increased or stays the same...
       */
      if (availability >= previousAvailability) {

        /**
         * and previous availability was not zero,
         * durations of all currently merging periods must be extended.
         * merging(time) = (duration + 1, amount)
         */
        if (previousAvailability > 0) {
          for (i <- 0 to time - 1) {
            if (merging(i) != null)
              extendPeriod(i)
          }
        }

        /**
         * If the availability is increased,
         * a new merging period must be added for the increase.
         * merging(time) = (1, increase)
         */
        if (availability > previousAvailability) {
          merging(time) = (1, availability - previousAvailability)
        }

        /**
         * If the availability is reduced...
         */
      } else { // availability < previousAvailability
        /**
         * ... to zero, all currently merging periods must be closed
         * (moved from merging to merged)
         */
        if (availability == 0) {
          for (i <- 0 to modulo - 1)
            if (merging(i) != null)
              merged(i) = merging(i) :: merged(i)
          merging = new Array[(Int, Int)](modulo)
          /**
           * ... to a strictly positive value,
           * the difference must be computed and
           * latest merging periods must be
           * - closed (if smaller) or
           * - divided (if bigger) while the difference is not zero
           * remaining merging periods must be incremented
           */
        } else { // 0 < availability < previousAvailability
          var diff = previousAvailability - availability

          /**
           * Divides the period starting at given time, i.e.:
           * - saves period (startDate, duration, diff) in merged periods
           * - adds period (startDate, duration + 1, amount - diff) to merging periods
           * - sets remaining amount difference to zero
           */
          def dividePeriod(startDate: Int) {
            val period = merging(startDate)
            merged(startDate) = (period._1, diff) :: merged(startDate)
            merging(startDate) = (period._1 + 1, period._2 - diff)
          }

          /**
           * Beginning with latest merging period,
           * closes or divides periods until all exceeding amount is removed.
           * If total exceeding amount < total amount,
           * remaining periods are extended.
           */
          for (i <- time - 1 to 0 by -1) {
            if (merging(i) != null) {
              if (diff > 0) {
                if (merging(i)._2 <= diff) {
                  val closedPeriod = closePeriod(i)
                  diff = diff - closedPeriod._2
                } else { // merging(i)._2 (amount) > diff
                  dividePeriod(i)
                  diff = 0
                }
              } else { // diff = 0
                extendPeriod(i)
              }
            }
          }
        }
      }
      previousAvailability = availability
    }

    /**
     * Closes last merging periods
     */
    for (i <- 0 to modulo - 1)
      if (merging(i) != null)
        closePeriod(i)

    //    merged.foreach(println)
    merged
  }
}
