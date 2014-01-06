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

import oscar.cbls.invariants.core.computation.{IntSetVar, IntVar, Model}
import oscar.cbls.invariants.lib.minmax.{ArgMinArray, ArgMaxArray}
import oscar.cbls.invariants.lib.logic.{Filter, DenseRef}
import oscar.visual.VisualFrame
import oscar.visual.plot.PlotLine
import oscar.cbls.scheduling.visu.Gantt
import oscar.cbls.modeling.Algebra._


class Planning(val model: Model, val maxduration: Int) {

  var resources: List[Resource] = List.empty
  var resourceCount: Int = 0
  /**called by resources registers it in the planning, returns an ID, which is the one of the resource*/
  def addResource(r: Resource): Int = {
    resources = r :: resources
    resourceCount += 1
    resourceCount - 1
  }

  var superActivity = false
  var Activities: List[Activity] = List.empty
  var activityCount: Int = 0
  /**called by activities registers it in the planning, returns an ID, which is the one of the activity*/
  def AddActivity(j: Activity): Int = {
    Activities = j :: Activities
    activityCount += 1
    activityCount - 1
  }

  var EarliestStartDates: Array[IntVar] = null
  var EarliestEndDates: Array[IntVar] = null
  var LatestStartDates: Array[IntVar] = null

  val MakeSpan: IntVar = IntVar(model, 0, maxduration, 0, "MakeSpan")
  var EarliestOvershotResources: IntSetVar = null
  var WorseOvershotResource: IntSetVar = null

  var ResourceArray: Array[Resource] = null
  var ActivityArray: Array[Activity] = null

  var SentinelActivity: Activity = null //a task that is added after all activities, to simplify algorithm construction

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
      if (j.isInstanceOf[SuperActivity]) superActivity = true
    }

    for (j <- Activities) {j.close()}

    DenseRef(ActivityArray.map(job => job.AllPrecedingActivities), ActivityArray.map(job => job.AllSucceedingActivities))

    MakeSpan <== SentinelActivity.EarliestStartDate

    ResourceArray = new Array[Resource](resourceCount)
    for (r <- resources) {
      ResourceArray(r.ResourceID) = r; r.close()
    }

    val WorseOvershootArray: Array[IntVar] = new Array[IntVar](resourceCount)
    for (r <- resources) {
      WorseOvershootArray(r.ResourceID) = r.overShoot
    }

    val ResourceWithOvershoot: IntSetVar = Filter(WorseOvershootArray)

    WorseOvershotResource = ArgMaxArray(WorseOvershootArray, ResourceWithOvershoot)
  }

  var gantt:Gantt = null
  var plot:PlotLine = null
  def displayVisualRendering(){
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

  override def toString: String = toAsciiArt

  def toAsciiArt: String = {
    var toreturn: String = ""
    def nStrings(N: Int, C: String): String = (if (N <= 0) "" else "" + C + nStrings(N - 1, C))
    def padToLength(s: String, l: Int) = (s + nStrings(l, " ")).substring(0, l)
    val activityList = Activities.filter(_!=SentinelActivity).sortWith((a, b) => a.EarliestStartDate.value < b.EarliestStartDate.value)
    val activityStrings = activityList.map(activity =>
    "" + padToLength(activity.name, 20) + ":" + "[" +
      padToLength("" + activity.EarliestStartDate.value, 4) + ";" + padToLength("" + activity.EarliestEndDate.value, 4) + "] " +
      (if (activity.duration.value == 1) nStrings(activity.EarliestStartDate.value, " ") + "#\n"
      else nStrings(activity.EarliestStartDate.value, " ") + "#" + nStrings(activity.duration.value - 2, "=") + "#\n"))

    activityStrings.mkString + MakeSpan + "\n"
  }

  def dependencies: String = {
    var toreturn: String = ""
    for (activity <- Activities.sortBy(t => t.EarliestStartDate.value)){
      for (t2 <- activity.AllSucceedingActivities.value if t2 != activity.ID && t2 != SentinelActivity.ID){
        val activity2 = ActivityArray(t2)
        if (activity2.AdditionalPredecessors.value.contains(activity.ID)){
          toreturn += activity.name + " -> " + activity2.name + "\n"
        }else{
          toreturn += activity.name + " ->> " + activity2.name + "\n"
        }
      }
    }
    toreturn
  }


  /**
   * Checks that a dependence from --> to can be added to the graph,
   * assuming that there is a resource conflict involving them
   * @param from
   * @param to
   * @return true if a dependence can be add, false otherwise.
   */
  def canAddPrecedenceAssumingResourceConflict(from:Activity, to:Activity):Boolean = {
    //this is not straigntworfward since there can be some SuperTasks.
    (from != to) && to.canAddPrecedence && ((!superActivity) || !isThereDependency(to,from))
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
      if (from.EarliestEndDate.value > target.EarliestStartDate.value){
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

  /**returns a list of pair of activity; precedences to kill
   * to add the new dependency newFrom -> newTo
   * without introducing a cycle involving newFrom -> newTo
   *
   * it computes a cut in the dag newTo -> newFrom involving only additional dependencies
   * @param newFrom
   * @param newTo
   * @return
   */
  def getDependencyToKillToAvoidCycle(newFrom:Activity, newTo:Activity):DependencyCleaner = {
    val from = newTo
    val to = newFrom
    if(from == to) return HardRockDependency()

    var MarkedActivities:List[Activity] = List.empty
    var DependenciesToKill:List[(Activity, Activity)] = List.empty
    /**marks all activities on the path linking From to To
     * all market activities are also added to MarkedActivities
      * return true if a path exist*/
    def MarkPathes(from:Activity, to:Activity):Boolean = {
      if(from.Mark) return true
      if (from == to){
        if (!from.Mark){
          from.Mark = true
          MarkedActivities = from :: MarkedActivities
        }
        return true
      }
      if (from.EarliestEndDate.value > to.EarliestStartDate.value){
        return false
      }

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
      for(prev <- to.getStartActivity.AdditionalPredecessors.value){
        val prevActivity:Activity = ActivityArray(prev)
        if (prevActivity.Mark){
          DependenciesToKill = (prevActivity,to) :: DependenciesToKill
          prevActivity.Mark = false
        }
      }
      for(prevActivity <- to.getStartActivity.StaticPredecessors){
        if (prevActivity.Mark){
          if (FindDependenciesToKill(from, prevActivity)){
            prevActivity.Mark = false
          }else{
            return false
          }
        }
      }
      true
    }

    MarkPathes(from.getStartActivity, to.getEndActivity)
    if(!FindDependenciesToKill(from.getStartActivity, to.getEndActivity)){
      for (t <- MarkedActivities) t.Mark = false
      HardRockDependency()
    }else{
      for (t <- MarkedActivities) t.Mark = false
      DependenciesCanBeKilled(DependenciesToKill)
    }
  }

  /** removes all additional Activity precedences that are not tight
    */
  def clean(){
    for(t:Activity <- ActivityArray){
      t.removeNonTightAdditionalPredecessors()
    }
  }
}

abstract class DependencyCleaner(val canBeKilled:Boolean){
  def killDependencies(Verbose:Boolean = false){throw new Exception("cannot kill dependencies")}
}
case class HardRockDependency() extends DependencyCleaner(false)

case class DependenciesCanBeKilled(d:List[(Activity, Activity)]) extends DependencyCleaner(true){
  override def killDependencies(Verbose:Boolean = false){
    for ((a,b) <- d){
      b.removeDynamicPredecessor(a,Verbose)
    }
  }
  def restoreDependencies(){
    for ((a,b) <- d){
      b.addDynamicPredecessor(a)
    }
  }
}