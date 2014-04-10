/*******************************************************************************
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
 ******************************************************************************/
package oscar.cbls.scheduling.model

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.invariants.core.computation.{CBLSIntVar, CBLSSetVar}
import scala.Array
import oscar.cbls.invariants.lib.logic.{Cumulative, Filter}
import oscar.cbls.invariants.lib.minmax.{ArgMaxArray, MinSet}
import oscar.cbls.invariants.lib.numeric.Step
import oscar.cbls.scheduling.algo.ConflictSearch
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.modeling.Algebra._
import collection.SortedMap

/**Maintains the resource usage at all time
 * the resource listens to the tasks using it, and maintains its overshoot times, and first overshoot
 *
 * @param planning the [[oscar.cbls.scheduling.model.Planning]] where the task is located
 * @param maxAmount the available amount of this resource that is available throughout the planning
 * @param n the name of the resource, used to annotate the internal variables of the problem
  * @author renaud.delandtsheer@cetic.be
 */
case class CumulativeResource(planning: Planning, maxAmount: Int = 1, n: String = null)
  extends Resource(planning:Planning, n) with SearchEngineTrait {
  require(maxAmount >= 0) // The IntVar that store the useAmount would break if their domain of lb > ub.

  val useAmount = Array.tabulate(maxDuration+1)(t => CBLSIntVar(model, 0, Int.MaxValue, 0, s"use_amount_${name}_at_time_$t"))
  
  val HighestUseTracker = ArgMaxArray(useAmount)
  val HighestUsePositions: CBLSSetVar = HighestUseTracker
  val HighestUse = HighestUseTracker.getMax

  val overShoot: CBLSIntVar = HighestUse - maxAmount
  def worseOverShootTime: Int = HighestUsePositions.value.firstKey

  /** you need to eject one of these to solve the conflict */
  def conflictingActivities(t: Int): List[Activity] = {
    val conflictSet: List[(Activity, CBLSIntVar)] = ConflictSearch(
      0,
      activitiesAndUse(t),
      (use: Int, ActivityAndamount: (Activity, CBLSIntVar)) => use + ActivityAndamount._2.value,
      (use: Int, ActivityAndamount: (Activity, CBLSIntVar)) => use - ActivityAndamount._2.value,
      (use: Int) => use > maxAmount
    )

    conflictSet.map(_._1)
  }

  def close() {

    val tasks:Array[Activity] = ActivitiesAndUse.keys.toArray

    Cumulative(
      tasks.map(_.ID),
      tasks.map(_.earliestStartDate),
      tasks.map(_.duration),
      tasks.map(ActivitiesAndUse(_)),
      useAmount,
      use)
  }

  def toAsciiArt(headerLength: Int): String = {
    def nStrings(N: Int, C: String): String = (if (N <= 0) "" else "" + C + nStrings(N - 1, C))
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)

    var lines:List[String] = List.empty

    for(i <- 1 to maxAmount){
      //header
      lines =
        ("" + padToLength(if (i == maxAmount)n else "", 21)
        + "|" + padToLength("" + i, 9) + "| " + useAmount.toList.map((v:CBLSIntVar) => if(v.value >= i) "+" else " ").mkString + "\n"
        ):: lines
    }
    lines.mkString
  }
}
