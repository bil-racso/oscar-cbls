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
package oscar.cp.constraints

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.core.CPStore

class MaxSweepCumulative(cp: CPStore, allTasks : Array[CumulativeActivity], ub : Int, r : Int) extends SweepCumulativeA(cp, allTasks, Int.MinValue, ub, r, "MaxSweepCumulative") {

	val eventPointSeries = new Array[Event](nTasks*3)
	
	override def generateCheck(i : Int) {}
	
	override def generateProfileBad(i : Int) : Boolean = {
		
		if (tasks(i).minHeight > 0) {
			
			eventPointSeries(nEvents) = eventList(i).sBadProfile(0, tasks(i).minHeight)
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eBadProfile(0, tasks(i).minHeight)
			nEvents += 1
			
			return true
		}
		
		return false
	}
	
	override def generateProfileGood(i : Int) : Boolean = {
		
		if (tasks(i).minHeight < 0) {
			
			eventPointSeries(nEvents) = eventList(i).sGoodProfile(0, tasks(i).minHeight)
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eGoodProfile(0, tasks(i).minHeight)
			nEvents += 1
			
			return true
		}
		
		return false
	}
	
	override def consistencyCheck : Boolean = (capaSumHeight > ub)
	
	override def mandatoryCheck(t: Int) : Boolean = capaSumHeight - capaContrib(t) > ub
	
	override def forbidenCheck(t : Int) : Boolean = capaSumHeight - capaContrib(t) + tasks(t).minHeight > ub
}
