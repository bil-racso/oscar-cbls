package oscar.cp.scheduling

import oscar.cp.core.CPVarInt
import oscar.cp.core.Store

class ProdConsActivity(activity: Activity, resourceVar : CPVarInt, heightVar : CPVarInt, atEnd : Boolean) extends CumulativeActivity(activity, resourceVar, heightVar) {

	private val startVar = if (atEnd) activity.end else activity.start
	private val endVar   = CPVarInt(start.store, activity.store.horizon)
	private val durVar   = CPVarInt(start.store, 0 to activity.store.horizon)
	activity.store.add(activity.start + durVar == endVar)
    
    override def start = startVar
    override def end   = endVar
    override def dur   = durVar
    
	override def est = startVar.min
	override def lst = startVar.max
	override def ect = endVar.min
	override def lct = endVar.max
	
	override def minDuration = durVar.min
	override def maxDuration = durVar.max
		
	override def toString() = "dur: "+dur+ " in ["+est+","+lct+"[" + " height: " + heightVar + " resource: " + resourceVar
}

object ProdConsActivity {
	
	def apply(activity : Activity, resourceVar : Int, heightVar : Int) = {
		
		new ProdConsActivity(activity, CPVarInt(activity.scheduler, resourceVar), CPVarInt(activity.scheduler, heightVar), true)
	}
	
	def apply(activity : Activity, resourceVar : Int, heightVar : Int, atEnd : Boolean) = {
		
		new ProdConsActivity(activity, CPVarInt(activity.scheduler, resourceVar), CPVarInt(activity.scheduler, heightVar), atEnd)
	}
}