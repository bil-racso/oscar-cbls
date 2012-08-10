package oscar.cp.scheduling

import java.security.InvalidParameterException
import oscar.cp.core.CPVarInt

// Usage Types
private class UsageType
private object Needs  extends UsageType { override def toString = "need" }
private object Gives  extends UsageType { override def toString = "give" }
private object NeedsF extends UsageType { override def toString = "need forever" }
private object GivesF extends UsageType { override def toString = "give forever" }

trait AmountOfResource
class NeedsUsage(act : Activity, height : ImplicitVarInt)  extends AmountOfResource {
	
	def ofResources(res : ImplicitVarInt) = {
		
		val heightVar   = height.variable(act.scheduler)
		val resourceVar = res.variable(act.scheduler)
		
		new AlternativeUsage(act, heightVar, resourceVar)
	}
	
	def ofResources(res : CumulativeResource*) = {
		
		val heightVar   = height.variable(act.scheduler)
		val resourceVar = CPVarInt(act.scheduler, res.map(_.id).toArray)
		
		new AlternativeUsage(act, heightVar, resourceVar)
	}
	
	def ofResource(res : CumulativeResource) = {
		
		val scheduler = act.scheduler
		val cum = CumulativeActivity(act, res.id, height)
		res.addActivity(act, cum)
	}
}
class GivesUsage(act : Activity, height : ImplicitVarInt) extends AmountOfResource {
	
	def toResources(res : ImplicitVarInt) = {
		
		val heightVar   = height.opposite(act.scheduler)
		val resourceVar = res.variable(act.scheduler)
		
		new AlternativeUsage(act, heightVar, resourceVar)
	}
	
	def toResources(res : Array[CumulativeResource]) = {
		
		val heightVar   = height.opposite(act.scheduler)
		val resourceVar = CPVarInt(act.scheduler, res.map(_.id).toArray)
		
		new AlternativeUsage(act, heightVar, resourceVar)
	}
	
	def toResource(res : CumulativeResource) = {
		
		val scheduler = act.scheduler
		val cum = CumulativeActivity(act, res.id, height.opposite(scheduler))
		res.addActivity(act, cum)
	}
}

trait ResourceUsed
case class AlternativeUsage(act : Activity, height : CPVarInt, resource : CPVarInt) extends ResourceUsed {
	
	def in(set : AlternativeCumulativeResource) {
		val scheduler = act.scheduler
		val cum = CumulativeActivity(act, resource, height)
		set.addActivity(act, cum)
	}
}

