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

import collection.immutable.SortedSet
import oscar.cbls.invariants.core.computation.IntVar._
import oscar.cbls.invariants.core.computation.{IntSetVar, IntVar}
import oscar.cbls.invariants.lib.set.{Inter, Union}
import oscar.cbls.algebra.Algebra._
import oscar.cbls.invariants.lib.minmax.{MinArray, ArgMaxArray}

case class SuperTask(start: Task, end: Task, override val name: String = "")
  extends Task(new IntVar(start.planning.model, 0, start.planning.maxduration, start.duration.value, "duration of " + name),
    start.planning, name) {

  start precedes end

  override def post() {

    start.post()
    end.post()

    AdditionalPredecessors = start.AdditionalPredecessors

    AllPrecedingTasks = start.AllPrecedingTasks

    EarliestStartDate <== start.EarliestStartDate

    DefiningPredecessors = start.DefiningPredecessors

    PotentiallyKilledPredecessors = start.PotentiallyKilledPredecessors

    AllSucceedingTasks = new IntSetVar(planning.model, 0, planning.taskcount - 1, "succeeding_jobs")

    LatestEndDate <== end.LatestEndDate

    this.duration <== end.EarliestEndDate - start.EarliestStartDate

    //ParasiticPrecedences = SortedSet.empty[Int]
  }

  override def addDynamicPredecessor(t: Task) {
    start.addDynamicPredecessor(t)
  }

  override def removeDynamicPredecessor(t:Task){
    start.removeDynamicPredecessor(t)
  }
  override def getEndTask: Task = end.getEndTask
  override def getStartTask: Task = start.getStartTask

  override def addStaticPredecessor(j: Task) {
    start.addStaticPredecessor(j)
  }
}

case class Task(val duration: IntVar, val planning: Planning, val name: String = "") {
  val TaskID: Int = planning.AddTask(this)

  /**Used for marking algorithm. Must always be set to false between algorithm execution*/
  var Mark:Boolean =  false;

  override def toString: String = name

  var StaticPredecessors: List[Task] = List.empty

  def addStaticPredecessor(j: Task) {
    StaticPredecessors = j :: StaticPredecessors
  }

  def precedes(j: Task) {
    j.addStaticPredecessor(this)
  }

  def uses(n:IntVar):TaskAndAmount = TaskAndAmount(this,n)

  case class TaskAndAmount(t:Task, amount:IntVar){
    def ofResource(r:CumulativeResource){t.usesCumulativeResource(r, amount)}

    def ofResources(rr:CumulativeResource*){
      for (r <- rr){t.usesCumulativeResource(r, amount)}
    }
  }

  var Resources: List[(CumulativeResource, IntVar)] = List.empty

  /**use this method to add resource requirement to a task.
   * the task and the resource must be registered to the same planning
   * @param r a resource that the task uses
   * @param amount the amount of this resource that the task uses
   */
  def usesCumulativeResource(r: CumulativeResource, amount: IntVar) {
    Resources = (r, amount) :: Resources
    r.notifyUsedBy(this, amount)
  }

  var EarliestStartDate: IntVar = new IntVar(planning.model, 0,
    planning.maxduration, duration.value, "esd(" + name + ")")
  val EarliestEndDate: IntVar = new IntVar(planning.model, 0,
    planning.maxduration, duration.value, "eed(" + name + ")")
  EarliestEndDate <== EarliestStartDate + duration

  val LatestEndDate: IntVar = new IntVar(planning.model, 0,
    planning.maxduration, planning.maxduration, "led(" + name + ")")

  val LatestStartDate: IntVar = LatestEndDate - duration
  var AllSucceedingTasks: IntSetVar = null

  var AdditionalPredecessors: IntSetVar = null
  var AllPrecedingTasks: IntSetVar = null

  var DefiningPredecessors: IntSetVar = null
  var PotentiallyKilledPredecessors: IntSetVar = null

  def addDynamicPredecessor(t: Task) {
    AdditionalPredecessors :+= t.getEndTask.TaskID
  }

  def removeDynamicPredecessor(t:Task){
    AdditionalPredecessors :-= t.getEndTask.TaskID
  }

  def getEndTask: Task = this
  def getStartTask: Task = this

 // var ParasiticPrecedences:IntSetVar = null
  /**This method is called by the planning when all tasks are created*/
  def post() {
    if (AdditionalPredecessors == null){
      AdditionalPredecessors = new IntSetVar(planning.model, 0, planning.Tasks.size,
        "added predecessors of " + name, SortedSet.empty)

      val StaticPredecessorsID: SortedSet[Int] = SortedSet.empty[Int] ++ StaticPredecessors.map((j: Task) => j.TaskID)
      AllPrecedingTasks = Union(StaticPredecessorsID, AdditionalPredecessors)

      val argMax = ArgMaxArray(planning.EarliestEndDates, AllPrecedingTasks, 0)
      EarliestStartDate <== argMax.getMax

      DefiningPredecessors = argMax

      PotentiallyKilledPredecessors = Inter(DefiningPredecessors, AdditionalPredecessors)

      AllSucceedingTasks = new IntSetVar(planning.model, 0, planning.taskcount - 1, "succeeding_jobs")

      LatestEndDate <== MinArray(planning.LatestStartDates, AllSucceedingTasks, planning.maxduration)

     // ParasiticPrecedences = AdditionalPredecessors minus PotentiallyKilledPredecessors
    }
  }
}

