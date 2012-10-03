package oscar.cbls.scheduling

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

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
 * @param planning the planning where the task is located
 * @param MaxAmount the available amount of this resource that is available throughout the planning
 * @param name the name of the resource, used to annotate the internal variables of the problem
 */
class CumulativeResource(planning: Planning, val MaxAmount: Int, name: String = "") {
  val ResourceID = planning.AddRessource(this)

  /**The set of tasks using this resource at every position*/
  val Use: Array[IntSetVar] = (for (t <- 0 to planning.maxduration) yield
    new IntSetVar(planning.model, 0, Int.MaxValue, "Tasks_using_" + name + "_at_ime_" + t)).toArray

  val UseAmount: Array[IntVar] = (for (t <- 0 to planning.maxduration) yield
    new IntVar(planning.model, 0, Int.MaxValue, 0, "use_amount_" + name + "_at_time_" + t)).toArray

  val FirstOvershoot: IntVar = MinSet(Filter(UseAmount, x => x > MaxAmount))

  val HighestUseTracker = ArgMaxArray(UseAmount)
  val HighestUsePositions: IntSetVar = HighestUseTracker
  val HighestUse = HighestUseTracker.getMax

  var TasksAndUse: List[(Task, Int)] = List.empty

  /**called by task to register itself to the resource*/
  def notifyUsedBy(j: Task, amount: Int) {
    TasksAndUse = (j, amount) :: TasksAndUse
  }

  def getTasksAndUse(t:Int):List[(Task, Int)] = {
    TasksAndUse.filter((taskAndamount: (Task, Int)) => Use(t).value.contains(taskAndamount._1.TaskID))
  }
  
  def close() {
    val NbTasks = TasksAndUse.size
    val TaskIDs: Array[Int] = new Array[Int](NbTasks)
    val UseAmounts: Array[IntVar] = new Array[IntVar](NbTasks)
    val TaskDurations: Array[IntVar] = new Array[IntVar](NbTasks)
    val TaskStarts: Array[IntVar] = new Array[IntVar](NbTasks)
    var i = 0;
    for (taskanduse <- TasksAndUse) {
      TaskIDs(i) = taskanduse._1.TaskID
      UseAmounts(i) = taskanduse._2
      TaskDurations(i) = taskanduse._1.duration
      TaskStarts(i) = taskanduse._1.EarliestStartDate
      i += 1
    }

    Cumulative(TaskIDs, TaskStarts, TaskDurations, UseAmounts, UseAmount, Use)
  }
}
