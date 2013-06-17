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

import scala.math.max
import scala.math.min

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.core.CPStore

class BoundedSweepCumulative(cp: CPStore, allTasks : Array[CumulativeActivity], lb : Int, ub : Int, r : Int) extends SweepCumulativeA(cp, allTasks, lb, ub, r, "BoundedSweepCumulative") {

	val eventPointSeries = new Array[Event](nTasks*7)
	
	override def generateCheck(i : Int) {
		
		if (tasks(i).maxHeight < lb) {
					
			eventPointSeries(nEvents) = eventList(i).sCheck
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eCheck
			nEvents += 1
		}
	}
	
	override def generateProfileBad(i : Int) : Boolean = {
		
		val capaInc = max(0, tasks(i).minHeight)
		val consInc = min(0, tasks(i).maxHeight)
		
		if (capaInc != 0 || consInc != 0) {
			
			eventPointSeries(nEvents) = eventList(i).sBadProfile(consInc, capaInc)  
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eBadProfile(consInc, capaInc)  
			nEvents += 1
			
			return true
		}
		
		return false
	}
	
	override def generateProfileGood(i : Int) : Boolean = {
		
		val capaInc = min(0, tasks(i).minHeight)
		val consInc = max(0, tasks(i).maxHeight)
		
		if (capaInc != 0 || consInc != 0) {
			
			// Generates events		
			eventPointSeries(nEvents) = eventList(i).sGoodProfile(consInc, capaInc)  
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eGoodProfile(consInc, capaInc)  
			nEvents += 1
			
			return true
		}
		
		return false
	}
	
	override def consistencyCheck : Boolean = ((nCurrentTasks > 0 && consSumHeight < lb) || capaSumHeight > ub) 

	override def mandatoryCheck(t: Int) : Boolean = (nCurrentTasks != 0 && (consSumHeight - consContrib(t)) < lb) || (capaSumHeight - capaContrib(t) > ub)		
	
	override def forbidenCheck(t : Int) : Boolean = (consSumHeight - consContrib(t) + tasks(t).maxHeight < lb) || 
			   										(capaSumHeight - capaContrib(t) + tasks(t).minHeight > ub)
}
