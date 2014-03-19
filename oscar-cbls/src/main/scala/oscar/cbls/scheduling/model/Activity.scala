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

/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 * ****************************************************************************
 */

package oscar.cbls.scheduling.model

import collection.immutable.SortedSet
import oscar.cbls.invariants.core.computation.CBLSIntVar._
import oscar.cbls.invariants.core.computation.{ CBLSSetVar, CBLSIntVar }
import oscar.cbls.invariants.lib.set.{ Inter, Union }
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.lib.minmax.{ MinArray, ArgMaxArray }

object Activity {
  def apply(duration: CBLSIntVar, planning: Planning, name: String = "",
            shifter: (CBLSIntVar, CBLSIntVar) => CBLSIntVar = (a: CBLSIntVar, _) => a) =
    new Activity(duration, planning, name, shifter)

  implicit val ord: Ordering[Activity] = new Ordering[Activity] {
    def compare(o1: Activity, o2: Activity) = o1.ID - o2.ID
  }
}
/**
 *
 * @param duration
 * @param planning
 * @param name
 * @param shifter a function that builds a shifter. A shifter is a function: start,duration => shifted start, that postpones a starting date to avoid some impossibilities
 * @author renaud.delandtsheer@cetic.be
 */
class Activity(val duration: CBLSIntVar, val planning: Planning, val name: String = "",
               shifter: (CBLSIntVar, CBLSIntVar) => CBLSIntVar = (a: CBLSIntVar, _) => a) {
  val ID: Int = planning.addActivity(this)

  override def equals(obj: Any): Boolean = {
    obj match {
      case a: Activity => a.ID == ID
      case _ => false
    }
  }

  def canEqual(that: Any): Boolean = that.isInstanceOf[Activity]

  /**Used for marking algorithm. Must always be set to false between algorithm execution*/
  var mark: Boolean = false

  override def toString: String = name

  var staticPredecessors: List[Activity] = List.empty

  def addStaticPredecessor(j: Activity) {
    staticPredecessors = j :: staticPredecessors
  }

  def precedes(j: Activity) {
    j.addStaticPredecessor(this)
  }

  def uses(n: CBLSIntVar): ActivityAndAmount = ActivityAndAmount(this, n)

  case class ActivityAndAmount(t: Activity, amount: CBLSIntVar) {
    def ofResource(r: CumulativeResource) {
      t.usesCumulativeResource(r, amount)
    }

    def ofResources(rr: CumulativeResource*) {
      for (r <- rr) { t.usesCumulativeResource(r, amount) }
    }

    def ofResources(rr: Iterable[CumulativeResource]) {
      rr.foreach(t.usesCumulativeResource(_, amount))
    }
  }

  def removeNonTightAdditionalPredecessors() {
    for (iD: Int <- additionalPredecessors.value) {
      if (!potentiallyKilledPredecessors.value.contains(iD)) {
        additionalPredecessors :-= iD
      }
    }
  }

  var Resources: List[(CumulativeResource, CBLSIntVar)] = List.empty

  /**
   * use this method to add resource requirement to a activity.
   * the activity and the resource must be registered to the same planning
   * @param r a resource that the activity uses
   * @param amount the amount of this resource that the activity uses
   */
  def usesCumulativeResource(r: CumulativeResource, amount: CBLSIntVar) {
    Resources = (r, amount) :: Resources
    r.notifyUsedBy(this, amount)
  }

  def maxDuration = planning.maxDuration

  val earliestStartDate: CBLSIntVar = CBLSIntVar(planning.model, 0, maxDuration, 0,
      "esd(" + name + ")")
  val earliestEndDate: CBLSIntVar = CBLSIntVar(planning.model, 0, maxDuration, duration.value,
      "eed(" + name + ")")
  earliestEndDate <== earliestStartDate + duration

  val latestEndDate: CBLSIntVar = CBLSIntVar(planning.model, 0, maxDuration, maxDuration,
      "led(" + name + ")")

  val latestStartDate: CBLSIntVar = latestEndDate - duration
  var allSucceedingActivities: CBLSSetVar = null

  var additionalPredecessors: CBLSSetVar = null
  var allPrecedingActivities: CBLSSetVar = null

  var definingPredecessors: CBLSSetVar = null
  var potentiallyKilledPredecessors: CBLSSetVar = null

  def addDynamicPredecessor(t: Activity, verbose: Boolean = false) {
    if (verbose) println("added " + t + "->" + this)
    additionalPredecessors :+= t.getEndActivity.ID
  }

  def removeDynamicPredecessor(t: Activity, verbose: Boolean = false) {
    if (verbose) println("killed " + t + "->" + this)
    additionalPredecessors :-= t.getEndActivity.ID
  }

  def getEndActivity: Activity = this
  def getStartActivity: Activity = this

  def canAddPrecedence: Boolean = true

  // var ParasiticPrecedences:IntSetVar = null
  /**This method is called by the planning when all activities are created*/
  def close() {
    if (additionalPredecessors == null) {
      additionalPredecessors = new CBLSSetVar(planning.model, 0, planning.activities.size,
        "added predecessors of " + name, SortedSet.empty)

      val staticPredecessorsID: SortedSet[Int] = SortedSet.empty[Int] ++ staticPredecessors.map((j: Activity) => j.ID)
      allPrecedingActivities = Union(staticPredecessorsID, additionalPredecessors)

      val argMax = ArgMaxArray(planning.earliestEndDates, allPrecedingActivities, 0)
      earliestStartDate <== argMax.getMax

      definingPredecessors = argMax

      potentiallyKilledPredecessors = Inter(definingPredecessors, additionalPredecessors)

      allSucceedingActivities = new CBLSSetVar(planning.model, 0, planning.activityCount - 1,
        "succeeding_activities_of_" + name)

      latestEndDate <== MinArray(planning.latestStartDates, allSucceedingActivities,
          planning.maxDuration)
    }
  }
}

