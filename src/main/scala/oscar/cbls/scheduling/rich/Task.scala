package oscar.cbls.jobshop.rich

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

import collection.immutable.SortedSet
import oscar.cbls.invariants.core.computation.IntVar._
import oscar.cbls.invariants.core.computation.{IntSetVar, IntVar}
import oscar.cbls.invariants.lib.set.{Inter, Union}
import oscar.cbls.algebra.Algebra._
import oscar.cbls.invariants.lib.minmax.ArgMaxArray._
import oscar.cbls.invariants.lib.minmax.MinArray._
import oscar.cbls.invariants.lib.minmax.{MinArray, ArgMaxArray}

class SuperTask(start:Task, end:Task, planning:Planning,  override val name:String = "")
  extends Task(new IntVar(planning.model, 0, planning.maxduration, start.duration.value, "duration of " + name),
    planning, name){

  override def post(){
    super.post()
    this.duration <== start.EarliestStartDate - end.EarliestEndDate
  }

  override def addDynamicPredecessor(t:Task){
    start.addDynamicPredecessor(t)
  }
  
  override def getEndTask:Task = end.getEndTask
}

class Task(val duration: IntVar, planning: Planning, val name: String = "") {
  val TaskID: Int = planning.AddTask(this)

  override def toString: String = name

  var StaticPredecessors: List[Task] = List.empty

  def addStaticPredecessor(j: Task) {
    StaticPredecessors = j :: StaticPredecessors
  }

  var Resources: List[(Resource, Int)] = List.empty

  /**use this method to add resource requirement to a task.
   * the task and the resource must be registered to the same planning
   * @param r a resource that the task uses
   * @param amount the amount of this resource that the task uses
   */
  def addResource(r: Resource, amount: Int) {
    Resources = (r, amount) :: Resources
    r.notifyUsedBy(this, amount)
  }

  var EarliestStartDate: IntVar = null
  val EarliestEndDate: IntVar = new IntVar(planning.model, 0, planning.maxduration, duration.value, "eed(" + name + ")")

  val LatestEndDate: IntVar = new IntVar(planning.model, 0,
    planning.maxduration, planning.maxduration, "led(" + name + ")")

  val LatestStartDate: IntVar = LatestEndDate - duration
  var AllSucceedingTasks: IntSetVar = null

  var AdditionalPredecessors: IntSetVar = null
  var AllPrecedingTasks: IntSetVar = null

  var DefiningPredecessors: IntSetVar = null
  var PotentiallyKilledPredecessors: IntSetVar = null

  def addDynamicPredecessor(t:Task){
    AdditionalPredecessors :+= t.getEndTask.TaskID
  }

  def getEndTask:Task = this

  /**This method is called by the planning when all tasks are created*/
  def post() {

    AdditionalPredecessors = new IntSetVar(planning.model, 0, planning.Tasks.size,
      "added predecessors of " + name, SortedSet.empty)

    val StaticPredecessorsID: SortedSet[Int] = SortedSet.empty[Int] ++ StaticPredecessors.map((j: Task) => j.TaskID)
    AllPrecedingTasks = Union(StaticPredecessorsID, AdditionalPredecessors)

    val argMax = ArgMaxArray(planning.EarliestEndDates, AllPrecedingTasks, 0)
    EarliestStartDate = argMax.getMax
    EarliestEndDate <== EarliestStartDate + duration

    DefiningPredecessors = argMax

    PotentiallyKilledPredecessors = Inter(DefiningPredecessors, AdditionalPredecessors)

    AllSucceedingTasks = new IntSetVar(planning.model, 0, planning.taskcount - 1, "succeeding_jobs")

    LatestEndDate <== MinArray(planning.LatestStartDates, AllSucceedingTasks, planning.maxduration)
  }
}


