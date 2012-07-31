package oscar.cp.constraints

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.core.Store

class MaxSweepCumulative(cp: Store, allTasks : Array[CumulativeActivity], ub : Int, r : Int) extends SweepCumulativeA(cp, allTasks, Int.MinValue, ub, r, "MaxSweepCumulative") {

	val eventPointSeries = new Array[Event](nTasks*5)
	
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
