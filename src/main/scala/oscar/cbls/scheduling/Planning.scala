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
import oscar.cbls.invariants.lib.minmax.{MinArray, ArgMinArray, ArgMaxArray}
import oscar.cbls.invariants.lib.logic.{Filter, DenseRef}
import oscar.cbls.algebra.Algebra._;

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
    val TaskssNoSentinel = Tasks
    SentinelTask = new Task(0, this, "SentinelTask")
    SentinelTask.LatestEndDate := maxduration

    for (task <- TaskssNoSentinel) {
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

  override def toString: String = {
    var toreturn: String = ""
    for (j <- Tasks.sortWith((a, b) => a.EarliestStartDate.value < b.EarliestStartDate.value) if j != SentinelTask) {
      toreturn += "" + j.name + "[" + j.EarliestStartDate.value + ";" + j.EarliestEndDate.value + "]" + "\n"
    }
    toreturn += MakeSpan
    toreturn
  }

  def toAsciiArt: String = {
    //TODO: duration is not an IntVAR
    var toreturn: String = ""
    def nStrings(N: Int, C: String): String = (if (N <= 0) "" else "" + C + nStrings(N - 1, C))
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
    for (j <- Tasks.sortWith((a, b) => a.EarliestStartDate.value < b.EarliestStartDate.value) if j != SentinelTask) {
      toreturn += "" + padToLength(j.name, 20) + ":" + "[" +
        padToLength("" + j.EarliestStartDate.value, 4) + ";" + padToLength("" + j.EarliestEndDate.value, 4) + "] " +
        (if (j.duration == 1) nStrings(j.EarliestStartDate.value, " ") + "#\n"
        else nStrings(j.EarliestStartDate.value, " ") + "#" + nStrings(j.duration.value - 2, "=") + "#\n")
    }
    toreturn += MakeSpan
    toreturn + "\n"
  }
}
