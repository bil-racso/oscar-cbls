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
package oscar.cbls.business.scheduling.model
/*
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *            Yoann Guyot
 * ****************************************************************************
 */

import oscar.cbls._
import oscar.cbls.algo.conflict.ConflictSearch
import oscar.cbls.lib.invariant.logic.Cumulative
import oscar.cbls.lib.invariant.minmax.{ArgMax, Max}
import oscar.cbls.lib.search.LinearSelectors

import scala.collection.SortedMap

/**
 * Maintains the resource usage at all time
 * the resource listens to the tasks using it, and maintains its overshoot times, and first overshoot
 *
 * @param planning the [[oscar.cbls.business.scheduling.model.Planning]] where the task is located
 * @param maxAmount the available amount of this resource that is available throughout the planning
 * @param name the name of the resource, used to annotate the internal variables of the problem
 * @author renaud.delandtsheer@cetic.be
 */
class CumulativeResource(planning: Planning, val maxAmount: Long = 1L, name: String = null)
  extends Resource(planning: Planning, name) with LinearSelectors {
  require(maxAmount >= 0L) // The IntVar that store the useAmount would break if their domain of lb > ub.

  val useAmount = Array.tabulate(maxDuration + 1L)(t => CBLSIntVar(model, 0L, 0L to Long.MaxValue, s"use_amount_${name}_at_time_$t"))
  var activitiesAndUse: SortedMap[Activity, IntValue] = SortedMap.empty

  val HighestUsePositions = ArgMax(useAmount)
  val HighestUse = Max(useAmount)

  val overShoot = HighestUse - maxAmount
  def worseOverShootTime: Long = HighestUsePositions.value.firstKey

  /**called by activities to register itself to the resource*/
  def notifyUsedBy(j: Activity, amount: IntValue) {
    if (activitiesAndUse.isDefinedAt(j)) {
      activitiesAndUse += ((j, activitiesAndUse.get(j).get + amount))
    } else {
      activitiesAndUse += ((j, amount))
    }
  }

  def activitiesAndUse(t: Long): List[(Activity, IntValue)] = {
    use(t).value.toList.map((a: Long) => {
      val activity: Activity = planning.activityArray(a);
      (activity, activitiesAndUse(activity))
    })
  }

  /**these are the activities that you can use for ejecting one of the conflicting activities*/
  def baseActivityForEjection(t: Long): Iterable[Activity] = {
    activitiesAndUse(t).map(_._1)
  }

  /** you need to eject one of these to solve the conflict */
  def conflictingActivities(t: Long): List[Activity] = {
    val conflictSet: List[(Activity, IntValue)] = ConflictSearch(
      0L,
      activitiesAndUse(t),
      (use: Long, ActivityAndamount: (Activity, IntValue)) => use + ActivityAndamount._2.value,
      (use: Long, ActivityAndamount: (Activity, IntValue)) => use - ActivityAndamount._2.value,
      (use: Long) => use > maxAmount)

    conflictSet.map(_._1)
  }

  def close() {

    val tasks: Array[Activity] = activitiesAndUse.keys.toArray

    Cumulative(
      tasks.map(_.ID),
      tasks.map(_.earliestStartDate),
      tasks.map(_.duration),
      tasks.map(activitiesAndUse(_)),
      useAmount,
      use)
  }

  def toAsciiArt(headerLength: Long): String = {
    def nStrings(N: Long, C: String): String = if (N <= 0L) "" else "" + C + nStrings(N - 1L, C)
    def padToLength(s: String, l: Long) = (s + nStrings(l, " ")).substring(0L, l)

    var lines: List[String] = List.empty

    for (i <- 1L to maxAmount) {
      //header
      lines =
        ("" + padToLength(if (i == maxAmount) name else "", 21L)
          + "|" + padToLength("" + i, 9L) + "| " + useAmount.toList.map((v: CBLSIntVar) => if (v.value >= i) "+" else " ").mkString + "\n") :: lines
    }
    lines.mkString
  }
}

object CumulativeResource {
  def apply(planning: Planning, maxAmount: Long = 1L, n: String = null) = {
    new CumulativeResource(planning, maxAmount, n)
  }
}

/**
 * Lets one declare a variable resource.
 * Typically: resource which vary with week days.
 *
 * availabilities(t) = amount of the resource available at time t
 * If the size of availabilities is less than the horizon, it will be
 * used modulo this size:
 *    let t be such that: availabilities.size <= t <= horizon
 *    the amount of resource at time t will be:
 *    availabilities(t % availabilities.size)
 *
 * Important: one should use planning trait VariablesResources
 *            to add such kind of resources
 *
 * Technically: it is a CumulativeResource using availabilities.max as
 * maximum amount, and restrictions for each time t, so that the actual
 * availability is the expected one.
 * @author yoann.guyot@cetic.be
 */
case class VariableResource(planning: Planning with VariableResources,
                            availabilities: Array[Long],
                            override val name: String = null)
  extends CumulativeResource(planning, availabilities.max, name) {

  val restrictionProfile = mergePeriods(availabilities.map(maxAmount - _))

  for (time <- 0L to maxDuration) {
    val timeModulo = time % availabilities.length
    val profile = restrictionProfile(timeModulo)
    if (profile.nonEmpty) {
      profile.foreach(applyRestrictionAt(time, _))
    }
  }

  /**
   * One should use this function to get the "restricted" usage
   * of the variable resource at time t (which is actually its real usage).
   * @author yoann.guyot@cetic.be
   */
  def restrictedUsage(time: Long) = {
    useAmount(time).value - restrictionAt(time)
  }

  /**
   * This function produces a list of availability task specifications,
   * in the form of an array: merged(startDate) = [(task1_duration, task1_amount), ...]
   * where availabilities are "merged" when equal in several positions
   * of the given availabilities array.
   * @author yoann.guyot@cetic.be
   */
  private def mergePeriods(availabilities: Array[Long]): Array[List[(Long, Long)]] = {
    val modulo = availabilities.length
    var merged: Array[List[(Long, Long)]] = Array.fill(modulo)(List())
    var merging = new Array[(Long, Long)](modulo)

    /**
     * Extends the merging period starting at given time,
     * i.e. increments period's duration.
     */
    def extendPeriod(startDate: Long) {
      val period = merging(startDate)
      merging(startDate) = (period._1 + 1L, period._2)
    }

    /**
     * Closes the period starting at given time, i.e.:
     * - saves the merging period in merged periods
     * - removes it from merging periods
     * - decrementing remaining amount difference by the removed period amount
     */
    def closePeriod(startDate: Long) = {
      val period = merging(startDate)
      merged(startDate) = period :: merged(startDate)
      merging(startDate) = null
      period
    }

    var previousAvailability = 0L
    /**
     * For each time, we evaluate the variation of availability between
     * the current time and the previous one.
     */
    for (time <- 0L until modulo) {
      val availability = availabilities(time)

      /**
       * If the availability is increased or stays the same...
       */
      if (availability >= previousAvailability) {

        /**
         * and previous availability was not zero,
         * durations of all currently merging periods must be extended.
         * merging(time) = (duration + 1L, amount)
         */
        if (previousAvailability > 0L) {
          for (i <- 0L until time) {
            if (merging(i) != null)
              extendPeriod(i)
          }
        }

        /**
         * If the availability is increased,
         * a new merging period must be added for the increase.
         * merging(time) = (1L, increase)
         */
        if (availability > previousAvailability) {
          merging(time) = (1L, availability - previousAvailability)
        }

        /**
         * If the availability is reduced...
         */
      } else { // availability < previousAvailability
        /**
         * ... to zero, all currently merging periods must be closed
         * (moved from merging to merged)
         */
        if (availability == 0L) {
          for (i <- 0L until modulo)
            if (merging(i) != null)
              merged(i) = merging(i) :: merged(i)
          merging = new Array[(Long, Long)](modulo)
          /**
           * ... to a strictly positive value,
           * the difference must be computed and
           * latest merging periods must be
           * - closed (if smaller) or
           * - divided (if bigger) while the difference is not zero
           * remaining merging periods must be incremented
           */
        } else { // 0L < availability < previousAvailability
          var diff = previousAvailability - availability

          /**
           * Divides the period starting at given time, i.e.:
           * - saves period (startDate, duration, diff) in merged periods
           * - adds period (startDate, duration + 1L, amount - diff) to merging periods
           * - sets remaining amount difference to zero
           */
          def dividePeriod(startDate: Long) {
            val period = merging(startDate)
            merged(startDate) = (period._1, diff) :: merged(startDate)
            merging(startDate) = (period._1 + 1L, period._2 - diff)
          }

          /**
           * Beginning with latest merging period,
           * closes or divides periods until all exceeding amount is removed.
           * If total exceeding amount < total amount,
           * remaining periods are extended.
           */
          for (i <- time - 1L to 0L by -1L) {
            if (merging(i) != null) {
              if (diff > 0L) {
                if (merging(i)._2 <= diff) {
                  val closedPeriod = closePeriod(i)
                  diff = diff - closedPeriod._2
                } else { // merging(i)._2 (amount) > diff
                  dividePeriod(i)
                  diff = 0L
                }
              } else { // diff = 0L
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
    for (i <- 0L until modulo)
      if (merging(i) != null)
        closePeriod(i)

    //    merged.foreach(println)
    merged
  }

  /**
   * This function is used to restrict availabilities of the resource to the
   * real amount it is supposed to provide.
   * @author yoann.guyot@cetic.be
   */
  private def applyRestrictionAt(time: Long,
                                 restriction: (Long, Long)) {
    var (duration, occupation) = restriction

    /**
     * If necessary, the duration of the restriction is reduced to the
     * remaining time to the horizon.
     */
    if (time + duration > maxDuration) {
      duration = maxDuration - time
    }

    val restrictionTask =
      planning.resourceRestrictionTasks(time).find(_.duration.value == duration) match {
        /**
         * If no task already exists to tackle this restriction
         * (same start, same duration), then a new ad-hoc task is added.
         */
        case None =>
          val restrictionEnd = time + duration - 1L
          val restrictionTask = new NonMoveableActivity(
            time, duration, planning, "ResRestriction" + time + "to" + restrictionEnd)
          planning.resourceRestrictionTasks(time) =
            restrictionTask :: planning.resourceRestrictionTasks(time)
          //          println("ResReduc" + time + "->" + reducEnd + " added.")
          restrictionTask
        case Some(restrictTask) => restrictTask
      }

    restrictionTask.usesCumulativeResource(this, occupation)
    //    println("At " + time + ": " +
    //      occupation + " / " + resource.maxAmount + " of " + resource.name)
  }

  /**
   * Computes the total restriction applied at the given time.
   * (not only restrictions beginning at this time!)
   * @author yoann.guyot@cetic.be
   */
  private def restrictionAt(time: Long): Long = {
    val restrictions = (0L to time) flatMap { (i: Long) =>
      planning.resourceRestrictionTasks(i) map { (task: Activity) =>
        if (i + task.duration.value - 1L >= time && this.activitiesAndUse.contains(task))
          this.activitiesAndUse(task).value
        else 0L
      }
    }
    restrictions.sum
  }
}
*/