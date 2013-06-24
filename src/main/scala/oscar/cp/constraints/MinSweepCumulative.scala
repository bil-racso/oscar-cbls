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

class MinSweepCumulative(cp: CPStore, allTasks : Array[CumulativeActivity], lb : Int, r : Int) extends SweepCumulativeA(cp, allTasks, lb, Int.MaxValue, r, "MinSweepCumulative") {	
	
	val eventPointSeries = new Array[Event](nTasks*5)
	
	override def generateCheck(i : Int) {
		
		if (tasks(i).maxHeight < lb) {
					
			eventPointSeries(nEvents) = eventList(i).sCheck
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eCheck
			nEvents += 1
		}
	}
	
	override def generateProfileBad(i : Int) : Boolean = {
		
		if (tasks(i).maxHeight < 0) {
			
			eventPointSeries(nEvents) = eventList(i).sBadProfile(tasks(i).maxHeight, 0)  
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eBadProfile(tasks(i).maxHeight, 0)  
			nEvents += 1
			
			return true
		} 
		
		return false
	}
	
	override def generateProfileGood(i : Int) : Boolean = {
		
		if (tasks(i).maxHeight > 0) {
				
			eventPointSeries(nEvents) = eventList(i).sGoodProfile(tasks(i).maxHeight, 0)  
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eGoodProfile(tasks(i).maxHeight, 0)  
			nEvents += 1
			
			return true
		} 
		
		return false
	}
	
	override def consistencyCheck : Boolean = (nCurrentTasks > 0 && consSumHeight < lb) 
	
	override def mandatoryCheck(t: Int) : Boolean = (nCurrentTasks != 0 && (consSumHeight - consContrib(t)) < lb) 
	
	override def forbidenCheck(t : Int) : Boolean = (consSumHeight - consContrib(t) + tasks(t).maxHeight < lb) 
}
