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

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  ******************************************************************************/

package oscar.cbls.scheduling.model

import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar}
import oscar.cbls.modeling.Algebra._

/**
 *
 * @param start
 * @param end
 * @param name
 * @author renaud.delandtsheer@cetic.be
 */
class SuperActivity(start: Activity, end: Activity, override val name: String = "")
  extends Activity(CBLSIntVar(start.planning.model, 0, start.planning.maxduration, start.duration.value, "duration of " + name),
    start.planning, name) {

  start precedes end

  override def close() {

    start.close()
    end.close()

    AdditionalPredecessors = start.AdditionalPredecessors

    AllPrecedingActivities = start.AllPrecedingActivities

    EarliestStartDate <== start.EarliestStartDate

    DefiningPredecessors = start.DefiningPredecessors

    PotentiallyKilledPredecessors = start.PotentiallyKilledPredecessors

    AllSucceedingActivities = new CBLSSetVar(planning.model, 0, planning.activityCount - 1, "succeeding_jobs")

    LatestEndDate <== end.LatestEndDate

    this.duration <== end.EarliestEndDate - start.EarliestStartDate

    //ParasiticPrecedences = SortedSet.empty[Int]
  }

  override def addDynamicPredecessor(t: Activity,Verbose:Boolean = false) {
    start.addDynamicPredecessor(t,Verbose)
  }

  override def removeDynamicPredecessor(t:Activity,Verbose:Boolean = false){
    start.removeDynamicPredecessor(t,Verbose)
  }
  override def getEndActivity: Activity = end.getEndActivity
  override def getStartActivity: Activity = start.getStartActivity

  override def addStaticPredecessor(j: Activity) {
    start.addStaticPredecessor(j)
  }

  override def removeNonTightAdditionalPredecessors(){} //nothing to be done here because no such dependencies can exist

}

object SuperActivity {
  def apply(start: Activity, end: Activity, name: String) = new SuperActivity(start,end,name)
  def apply(start: Activity, end: Activity) = new SuperActivity(start,end,"SuperActivity(" + start + "," + end + ")")
}
