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

import oscar.cbls.invariants.core.computation.{IntSetVar, IntVar, Model}
import oscar.cbls.invariants.lib.minmax.{ArgMinArray, ArgMaxArray}
import oscar.cbls.invariants.lib.logic.{Filter, DenseRef}
import oscar.visual.{Plot2D, VisualFrame}
;

class Planning(val model: Model, val maxduration: Int) {

  var Ressources: List[CumulativeResource] = List.empty
  var ResourceCount: Int = 0
  /**called by resources registers it in the planning, returns an ID, which is the one of the resource*/
  def AddRessource(r: CumulativeResource): Int = {
    Ressources = r :: Ressources;
    ResourceCount += 1;
    ResourceCount - 1
  }

  var Tasks: List[Task] = List.empty
  var taskcount: Int = 0
  /**called by taskss registers it in the planning, returns an ID, which is the one of the tasks*/
  def AddTask(j: Task): Int = {
    Tasks = j :: Tasks;
    taskcount += 1;
    taskcount - 1
  }

  var EarliestStartDates: Array[IntVar] = null
  var EarliestEndDates: Array[IntVar] = null
  var LatestStartDates: Array[IntVar] = null

  val MakeSpan: IntVar = new IntVar(model, 0, maxduration, 0, "MakeSpan")
  var EarliestOvershotResources: IntSetVar = null
  var WorseOvershotResource: IntSetVar = null

  var ResourceArray: Array[CumulativeResource] = null
  var TaskArray: Array[Task] = null

  var SentinelTask: Task = null //a taks that is added after all taskss, to simplify algorithm construction

  def close() {
    val TasksNoSentinel = Tasks
    SentinelTask = new Task(0, this, "SentinelTask")
    SentinelTask.LatestEndDate := maxduration

    for (task <- TasksNoSentinel) {
      SentinelTask.addStaticPredecessor(task)
    }

    TaskArray = new Array[Task](taskcount)
    EarliestEndDates = new Array[IntVar](taskcount)
    EarliestStartDates = new Array[IntVar](taskcount)
    LatestStartDates = new Array[IntVar](taskcount)

    for (j <- Tasks) {
      TaskArray(j.TaskID) = j
      EarliestStartDates(j.TaskID) = j.EarliestStartDate
      EarliestEndDates(j.TaskID) = j.EarliestEndDate
      LatestStartDates(j.TaskID) = j.LatestStartDate
    }

    for (j <- Tasks) {j.post()}

    DenseRef(TaskArray.map(job => job.AllPrecedingTasks), TaskArray.map(job => job.AllSucceedingTasks))

    MakeSpan <== SentinelTask.EarliestStartDate

    ResourceArray = new Array[CumulativeResource](ResourceCount)
    for (r <- Ressources) {
      ResourceArray(r.ResourceID) = r; r.close()
    }

    val FirstOvershootArray: Array[IntVar] = new Array[IntVar](ResourceCount)
    for (r <- Ressources) {
      FirstOvershootArray(r.ResourceID) = r.FirstOvershoot
    }
    val ResourceWithOvershoot: IntSetVar = Filter(FirstOvershootArray, (date: Int) => date <= maxduration)
    EarliestOvershotResources = ArgMinArray(FirstOvershootArray, ResourceWithOvershoot)

    val WorseOvershootArray: Array[IntVar] = new Array[IntVar](ResourceCount)
    for (r <- Ressources) {
      WorseOvershootArray(r.ResourceID) = r.HighestUse
    }
    WorseOvershotResource = ArgMaxArray(WorseOvershootArray, ResourceWithOvershoot)
  }

  var gantt:Gantt = null
  var plot:Plot2D = null
  def getVisual{
    val frame  = new VisualFrame("Cumulative JobShop Problem", 1, 1)
    frame.setBounds(0,0,500,800)
    gantt = new Gantt(this)
    frame.createFrame("Gantt chart").add(gantt)
 //   plot = new Plot2D("makespan", "iterations", "makespan");
 //   frame.createFrame("progress").add(plot)

    frame.pack
    frame.setSize(1500,500)
  }

  def updateVisual{
    if (gantt!=null) gantt.update(1.0f, 30)
  }

  override def toString: String = {
    var toreturn: String = ""
    for (j <- Tasks.sortWith((a, b) => a.EarliestStartDate.value < b.EarliestStartDate.value) if j != SentinelTask) {
      toreturn += "" + j.name + "[" + j.EarliestStartDate.value + ";" + j.EarliestEndDate.value + "]" + "\n"
    }
    toreturn += MakeSpan
    toreturn
  }

  def toAsciiArt: String = {
    var toreturn: String = ""
    def nStrings(N: Int, C: String): String = (if (N <= 0) "" else "" + C + nStrings(N - 1, C))
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
    for (j <- Tasks.sortWith((a, b) => a.EarliestStartDate.value < b.EarliestStartDate.value) if j != SentinelTask) {
      toreturn += "" + padToLength(j.name, 20) + ":" + "[" +
        padToLength("" + j.EarliestStartDate.value, 4) + ";" + padToLength("" + j.EarliestEndDate.value, 4) + "] " +
        (if (j.duration.value == 1) nStrings(j.EarliestStartDate.value, " ") + "#\n"
        else nStrings(j.EarliestStartDate.value, " ") + "#" + nStrings(j.duration.value - 2, "=") + "#\n")
    }
    toreturn += MakeSpan
    toreturn + "\n"
  }

  def dependencies: String = {
    var toreturn: String = ""
    for (task <- Tasks.sortBy(t => t.EarliestStartDate.value)){
      for (t2 <- task.AllSucceedingTasks.value if t2 != task.TaskID && t2 != SentinelTask.TaskID){
        val task2 = TaskArray(t2)
        if (task2.AdditionalPredecessors.value.contains(task.TaskID)){
          toreturn += task.name + " -> " + task2.name + "\n"
        }else{
          toreturn += task.name + " ->> " + task2.name + "\n"
        }
      }
    }
    toreturn
  }


  /**
   * Checks taht a dependence from --> to can be added to the graph,
   * assuming that there is a resource conflict involving them
   * @param from
   * @param to
   * @return true if a dependence can be addd, false otherwise.
   */
  def canAddPrecedenceAssumingResourceConflict(from:Task,  to:Task):Boolean = {
    (from != to) & !isThereDependency(to,from)
  }

  /**Checks if there is a path leading from one task to another one
   * @param from
   * @param to
   * @return true if there is a path from 'from' to 'to', false otherwise
   */
  def isThereDependency(from:Task, to:Task):Boolean = {
    val target = to.getEndTask

    var Reached:List[Task] = List.empty

    /**PRE: from is a ground task. */
    def Search(from:Task):Boolean = {
      if (from == target) return true
      if (from.EarliestEndDate.value > to.EarliestStartDate.value){
        return false
      }

      if(from.Mark){return false}
      from.Mark = true
      Reached = from :: Reached
      for(next <- from.getStartTask.AllSucceedingTasks.value){
        val nextTask:Task = TaskArray(next)
        if (Search(nextTask)) return true
      }
      false
    }

    val toreturn = Search(from.getStartTask)
    for (task <- Reached) task.Mark = false
    toreturn
  }

  /**returns a list of pair of task; precedences to kill to make it possible to add the new dependency
   * 
   * @param from
   * @param to
   * @return
   */
  def getDependencyToKillToAvoidCycle(from:Task, to:Task):DependencyCleaner = {
    var MarkedTasks:List[Task] = List.empty
    var DependenciesToKill:List[(Task, Task)] = List.empty
    /**marks all tasks on the path linking From to To
     * all market tasks are also added to MArketTasks*/ 
    def MarkPathes(from:Task, to:Task):Boolean = {
      if (from == to) return true;
      if (from.EarliestEndDate.value > to.EarliestStartDate.value){
        return false
      }
      if(from.Mark){return true}
      for(next <- from.getStartTask.AllSucceedingTasks.value){
        val nextTask:Task = TaskArray(next)
        if (MarkPathes(nextTask, to)) from.Mark = true
      }
      if (from.Mark){
        MarkedTasks = from :: MarkedTasks
      }
      from.Mark
    }

    /**returns false if hard rock dependency, true if can be killed*/
    def FindDependenciesToKill(from:Task, to:Task) :Boolean = {
      if (from == to) return false
      if(!from.Mark){return true}
      for(next <- from.getStartTask.AdditionalPredecessors.value){
        val nextTask:Task = TaskArray(next)
        if (nextTask.Mark){
          DependenciesToKill = (from,nextTask) :: DependenciesToKill
          nextTask.Mark = false
        }
      }
      for(nextTask <- from.getStartTask.StaticPredecessors){
        if (nextTask.Mark){
          if (FindDependenciesToKill(nextTask, to)){
            nextTask.Mark = false
          }else{
            return false
          }
        }
      }
      true
    }

    MarkPathes(from.getStartTask, to.getEndTask)
    if(FindDependenciesToKill(from.getStartTask, to.getEndTask)){
      for (t <- MarkedTasks) t.Mark = false
      HardRockDependency()
    }else{
      for (t <- MarkedTasks) t.Mark = false
      DependenciesCanBeKilled(DependenciesToKill)
    }
  }
  
  abstract class DependencyCleaner()
  case class HardRockDependency() extends DependencyCleaner
  case class DependenciesCanBeKilled(d:List[(Task, Task)]) extends DependencyCleaner{
    def killDependencies{
      for ((a,b) <- d){
        b.removeDynamicPredecessor(a)
      }
    }
    def restoreDependencies{
      for ((a,b) <- d){
        b.addDynamicPredecessor(a)
      }
    }
  }
}

