package oscar.cp.scheduling

import oscar.cp.core.CPVarInt
import oscar.cp.core.Store

class ProdConsActivity(activity: Activity, resourceVar : CPVarInt, heightVar : CPVarInt) extends CumulativeActivity(activity, resourceVar, heightVar){

	val horizon = 4
	
	private val endVar = CPVarInt(start.store, horizon)
	private val durVar = endVar.minus(start)
	
    override def end = endVar
    override def dur = durVar
	override def ect = endVar.min
	override def lct = endVar.max
	
	override def minDuration = durVar.min
	override def maxDuration = durVar.max
	
	override def toString() = activity + " height: " + heightVar
}

object ProdConsActivity {
	
	def apply(activity : Activity, resourceVar : Int, heightVar : Int) = {
		
		new ProdConsActivity(activity, CPVarInt(activity.scheduler, resourceVar), CPVarInt(activity.scheduler, heightVar))
	}
	
	def apply(activity : Activity, resourceVar : CPVarInt, heightVar : Int) = {
		
		new ProdConsActivity(activity, resourceVar, CPVarInt(activity.scheduler, heightVar))
	}
	
	def apply(activity : Activity, resourceVar : Int, heightVar : CPVarInt) = {
		
		new ProdConsActivity(activity, CPVarInt(activity.scheduler, resourceVar), heightVar)
	}
	
	def apply(activity : Activity, resourceVar : CPVarInt, heightVar : CPVarInt) = {
		
		new ProdConsActivity(activity, resourceVar, heightVar)
	}
}