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

  var Activities: List[Activity] = List.empty
  var activityCount: Int = 0
  /**called by activities registers it in the planning, returns an ID, which is the one of the activity*/
  def AddActivity(j: Activity): Int = {
    Activities = j :: Activities;
    activityCount += 1;
    activityCount - 1
  }

  var EarliestStartDates: Array[IntVar] = null
  var EarliestEndDates: Array[IntVar] = null
  var LatestStartDates: Array[IntVar] = null

  val MakeSpan: IntVar = new IntVar(model, 0, maxduration, 0, "MakeSpan")
  var EarliestOvershotResources: IntSetVar = null
  var WorseOvershotResource: IntSetVar = null

  var ResourceArray: Array[CumulativeResource] = null
  var ActivityArray: Array[Activity] = null

  var SentinelActivity: Activity = null //a taks that is added after all activities, to simplify algorithm construction

  def close() {
    val ActivitiesNoSentinel = Activities
    SentinelActivity = new Activity(0, this, "SentinelActivity")
    SentinelActivity.LatestEndDate := maxduration

    for (a <- ActivitiesNoSentinel) {
      SentinelActivity.addStaticPredecessor(a)
    }

    ActivityArray = new Array[Activity](activityCount)
    EarliestEndDates = new Array[IntVar](activityCount)
    EarliestStartDates = new Array[IntVar](activityCount)
    LatestStartDates = new Array[IntVar](activityCount)

    for (j <- Activities) {
      ActivityArray(j.ID) = j
      EarliestStartDates(j.ID) = j.EarliestStartDate
      EarliestEndDates(j.ID) = j.EarliestEndDate
      LatestStartDates(j.ID) = j.LatestStartDate
    }

    for (j <- Activities) {j.post()}

    DenseRef(ActivityArray.map(job => job.AllPrecedingActivities), ActivityArray.map(job => job.AllSucceedingActivities))

    MakeSpan <== SentinelActivity.EarliestStartDate

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
  def getVisual(){
    val frame  = new VisualFrame("Cumulative JobShop Problem", 1, 1)
    frame.setBounds(0,0,500,800)
    gantt = new Gantt(this)
    frame.createFrame("Gantt chart").add(gantt)
 //   plot = new Plot2D("makespan", "iterations", "makespan");
 //   frame.createFrame("progress").add(plot)

    frame.pack
    frame.setSize(1500,500)
  }

  def updateVisual(){
    if (gantt!=null) gantt.update(1.0f, 30)
  }

  override def toString: String = {
    var toreturn: String = ""
    for (j <- Activities.sortWith((a, b) => a.EarliestStartDate.value < b.EarliestStartDate.value) if j != SentinelActivity) {
      toreturn += "" + j.name + "[" + j.EarliestStartDate.value + ";" + j.EarliestEndDate.value + "]" + "\n"
    }
    toreturn += MakeSpan
    toreturn
  }

  def toAsciiArt: String = {
    var toreturn: String = ""
    def nStrings(N: Int, C: String): String = (if (N <= 0) "" else "" + C + nStrings(N - 1, C))
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
    for (j <- Activities.sortWith((a, b) => a.EarliestStartDate.value < b.EarliestStartDate.value) if j != SentinelActivity) {
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
    for (activity <- Activities.sortBy(t => t.EarliestStartDate.value)){
      for (t2 <- activity.AllSucceedingActivities.value if t2 != activity.ID && t2 != SentinelActivity.ID){
        val activity = ActivityArray(t2)
        if (activity.AdditionalPredecessors.value.contains(activity.ID)){
          toreturn += activity.name + " -> " + activity.name + "\n"
        }else{
          toreturn += activity.name + " ->> " + activity.name + "\n"
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
  def canAddPrecedenceAssumingResourceConflict(from:Activity,  to:Activity):Boolean = {
    (from != to) & !isThereDependency(to,from)
  }

  /**Checks if there is a path leading from one activity to another one
   * @param from
   * @param to
   * @return true if there is a path from 'from' to 'to', false otherwise
   */
  def isThereDependency(from:Activity, to:Activity):Boolean = {
    val target = to.getEndActivity

    var Reached:List[Activity] = List.empty

    /**PRE: from is a ground activity. */
    def Search(from:Activity):Boolean = {
      if (from == target) return true
      if (from.EarliestEndDate.value > to.EarliestStartDate.value){
        return false
      }

      if(from.Mark){return false}
      from.Mark = true
      Reached = from :: Reached
      for(next <- from.getStartActivity.AllSucceedingActivities.value){
        val activity:Activity = ActivityArray(next)
        if (Search(activity)) return true
      }
      false
    }

    val toreturn = Search(from.getStartActivity)
    for (activity <- Reached) activity.Mark = false
    toreturn
  }

  /**returns a list of pair of activity; precedences to kill to make it possible to add the new dependency
   * 
   * @param from
   * @param to
   * @return
   */
  def getDependencyToKillToAvoidCycle(from:Activity, to:Activity):DependencyCleaner = {
    var MarkedActivities:List[Activity] = List.empty
    var DependenciesToKill:List[(Activity, Activity)] = List.empty
    /**marks all activities on the path linking From to To
     * all market activities are also added to MArkekActivities*/
    def MarkPathes(from:Activity, to:Activity):Boolean = {
      if (from == to) return true;
      if (from.EarliestEndDate.value > to.EarliestStartDate.value){
        return false
      }
      if(from.Mark){return true}
      for(next <- from.getStartActivity.AllSucceedingActivities.value){
        val nextActivity:Activity = ActivityArray(next)
        if (MarkPathes(nextActivity, to)) from.Mark = true
      }
      if (from.Mark){
        MarkedActivities = from :: MarkedActivities
      }
      from.Mark
    }

    /**returns false if hard rock dependency, true if can be killed*/
    def FindDependenciesToKill(from:Activity, to:Activity) :Boolean = {
      if (from == to) return false
      if(!from.Mark){return true}
      for(next <- from.getStartActivity.AdditionalPredecessors.value){
        val nextActivity:Activity = ActivityArray(next)
        if (nextActivity.Mark){
          DependenciesToKill = (from,nextActivity) :: DependenciesToKill
          nextActivity.Mark = false
        }
      }
      for(nextActivity <- from.getStartActivity.StaticPredecessors){
        if (nextActivity.Mark){
          if (FindDependenciesToKill(nextActivity, to)){
            nextActivity.Mark = false
          }else{
            return false
          }
        }
      }
      true
    }

    MarkPathes(from.getStartActivity, to.getEndActivity)
    if(FindDependenciesToKill(from.getStartActivity, to.getEndActivity)){
      for (t <- MarkedActivities) t.Mark = false
      HardRockDependency()
    }else{
      for (t <- MarkedActivities) t.Mark = false
      DependenciesCanBeKilled(DependenciesToKill)
    }
  }
  
  abstract class DependencyCleaner()
  case class HardRockDependency() extends DependencyCleaner
  case class DependenciesCanBeKilled(d:List[(Activity, Activity)]) extends DependencyCleaner{
    def killDependencies(){
      for ((a,b) <- d){
        b.removeDynamicPredecessor(a)
      }
    }
    def restoreDependencies(){
      for ((a,b) <- d){
        b.addDynamicPredecessor(a)
      }
    }
  }
}

