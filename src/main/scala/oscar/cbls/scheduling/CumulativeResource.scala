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
package oscar.cbls.scheduling

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.invariants.core.computation.{IntVar, IntSetVar}
import scala.Array
import oscar.cbls.invariants.lib.logic.{Cumulative, Filter}
import oscar.cbls.invariants.lib.minmax.{ArgMaxArray, MinSet}

/**Maintains the resource usage at all time
 * the resource listens to the tasks using it, and maintains its overshoot times, and first overshoot
 *
 * @param planning the [[oscar.cbls.scheduling.Planning]] where the task is located
 * @param MaxAmount the available amount of this resource that is available throughout the planning
 * @param name the name of the resource, used to annotate the internal variables of the problem
 */
case class CumulativeResource(planning: Planning, MaxAmount: Int = 1, n: String = null) {
  require(MaxAmount >= 0) // The IntVar that store the useAmount would break if their domain of lb > ub.
  val ResourceID = planning.addResource(this)
  val name = Option(n) getOrElse s"Resource $ResourceID"
  
  private val model = planning.model
  private val maxDuration = planning.maxduration

  /**The set of activities using this resource at every position*/
  val use = Array.tabulate(maxDuration)(t => new IntSetVar(model, 0, Int.MaxValue, s"use_amount_${name}_at_time_${t}"))
  val useAmount = Array.tabulate(maxDuration)(t => IntVar(model, 0, Int.MaxValue, 0, s"use_amount_${name}_at_time_${t}"))
 
  val FirstOvershoot: IntVar = MinSet(Filter(useAmount, x => x > MaxAmount))
  
  val HighestUseTracker = ArgMaxArray(useAmount)
  val HighestUsePositions: IntSetVar = HighestUseTracker
  val HighestUse = HighestUseTracker.getMax

  var ActivitiesAndUse: List[(Activity, IntVar)] = List.empty

  /**called by activities to register itself to the resource*/
  def notifyUsedBy(j: Activity, amount: IntVar) {
    ActivitiesAndUse = (j, amount) :: ActivitiesAndUse
  }

  def getActivitiesAndUse(t:Int):List[(Activity, IntVar)] = {
    ActivitiesAndUse.filter((x: (Activity, IntVar)) => use(t).value.contains(x._1.ID))
  }
  
  def close() {
    val NbTasks = ActivitiesAndUse.size
   
    val taskIDs = Array.tabulate(NbTasks)(i => ActivitiesAndUse(i)._1.ID)
    val useAmounts = Array.tabulate(NbTasks)(i => ActivitiesAndUse(i)._2)
    val taskDurations = Array.tabulate(NbTasks)(i => ActivitiesAndUse(i)._1.duration)
    val taskStarts = Array.tabulate(NbTasks)(i => ActivitiesAndUse(i)._1.EarliestStartDate)
   
    Cumulative(taskIDs, taskStarts, taskDurations, useAmounts, useAmount, use)
  }
}
