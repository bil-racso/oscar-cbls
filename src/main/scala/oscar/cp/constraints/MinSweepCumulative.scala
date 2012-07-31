package oscar.cp.constraints

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.core.Store

class MinSweepCumulative(cp: Store, allTasks : Array[CumulativeActivity], lb : Int, r : Int) extends SweepCumulativeA(cp, allTasks, lb, Int.MaxValue, r, "MinSweepCumulative") {	
	
	val eventPointSeries = new Array[Event](nTasks*7)
	
	override def generateCheck(i : Int) {
		
		if (tasks(i).maxResource < lb) {
					
			eventPointSeries(nEvents) = eventList(i).sCheck
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eCheck
			nEvents += 1
		}
	}
	
	override def generateProfileBad(i : Int) : Boolean = {
		
		if (tasks(i).maxResource < 0) {
			
			eventPointSeries(nEvents) = eventList(i).sBadProfile(tasks(i).maxResource, 0)  
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eBadProfile(tasks(i).maxResource, 0)  
			nEvents += 1
			
			return true
		} 
		
		return false
	}
	
	override def generateProfileGood(i : Int) : Boolean = {
		
		if (tasks(i).maxResource > 0) {
				
			eventPointSeries(nEvents) = eventList(i).sGoodProfile(tasks(i).maxResource, 0)  
			nEvents += 1
			eventPointSeries(nEvents) = eventList(i).eGoodProfile(tasks(i).maxResource, 0)  
			nEvents += 1
			
			return true
		} 
		
		return false
	}
	
	override def consistencyCheck : Boolean = (nCurrentTasks > 0 && consSumHeight < lb) 
	
	override def mandatoryCheck(t: Int) : Boolean = (nCurrentTasks != 0 && (consSumHeight - consContrib(t)) < lb) 
	
	override def forbidenCheck(t : Int) : Boolean = (consSumHeight - consContrib(t) + tasks(t).maxResource < lb) 
}
