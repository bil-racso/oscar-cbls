package oscar.cp.scheduling

import java.security.InvalidParameterException
import oscar.cp.core.CPVarInt

// Usage Types
private class UsageType
private object Needs  extends UsageType { override def toString = "need" }
private object Gives  extends UsageType { override def toString = "give" }
private object NeedsF extends UsageType { override def toString = "need forever" }
private object GivesF extends UsageType { override def toString = "give forever" }

class AmountOfResource(val act : Activity, val height : ImplicitVarInt) {
	
}

class NeedsUsage(act : Activity, height : ImplicitVarInt) extends AmountOfResource(act, height) {
	
	def ofResources(res : ImplicitVarInt) = {
		
		val cum = CumulativeActivity(act, res, height)
		new AlternativeCumulativeUsage(act, cum)
	}
	
	def ofResources(res : CumulativeResource*) = {
		
		val cum = CumulativeActivity(act, res.map(_.id).toArray, height)
		new AlternativeCumulativeUsage(act, cum)
	}
	
	def ofResource(res : CumulativeResource) = {
		
		val scheduler = act.scheduler
		val cum = CumulativeActivity(act, res.id, height)
		res.addActivity(act, cum)
	}
}

class NeedsFUsage(act : Activity, height : ImplicitVarInt, atEnd : Boolean) extends AmountOfResource(act, height) {
	
	def ofResources(res : ImplicitVarInt) = {
		
		val cum = ProdConsActivity(act, res, height, atEnd)
		new AlternativeCumulativeUsage(act, cum)
	}
	
	def ofResources(res : CumulativeResource*) = {
		
		val cum = ProdConsActivity(act, res.map(_.id).toArray, height, atEnd)
		new AlternativeCumulativeUsage(act, cum)
	}
	
	def ofResource(res : CumulativeResource) = {
		
		val cum = ProdConsActivity(act, res.id, height, atEnd)
		res.addActivity(act, cum)
	}
}

class GivesUsage(act : Activity, height : ImplicitVarInt) extends AmountOfResource(act, height) {
	
	def toResources(res : ImplicitVarInt) = {
		
		val cum = CumulativeActivity(act, res, -height)
		new AlternativeCumulativeUsage(act, cum)
	}
	
	def toResources(res : CumulativeResource*) = {
		
		val cum = CumulativeActivity(act, res.map(_.id).toArray, -height)
		new AlternativeCumulativeUsage(act, cum)
	}
	
	def toResource(res : CumulativeResource) = {
		
		val cum = CumulativeActivity(act, res.id, -height)
		res.addActivity(act, cum)
	}
}

class GivesFUsage(act : Activity, height : ImplicitVarInt, atEnd : Boolean) extends AmountOfResource(act, height) {
	
	def toResources(res : ImplicitVarInt) = {
		
		val cum = ProdConsActivity(act, res, -height, atEnd)
		new AlternativeCumulativeUsage(act, cum)
	}
	
	def toResources(res : CumulativeResource*) = {
		
		val cum = ProdConsActivity(act, res.map(_.id).toArray, -height, atEnd)
		new AlternativeCumulativeUsage(act, cum)
	}
	
	def toResource(res : CumulativeResource) = {
		
		val scheduler = act.scheduler
		val cum = ProdConsActivity(act, res.id, -height, atEnd)
		res.addActivity(act, cum)
	}
}

class AlternativeCumulativeUsage(act : Activity, cum : CumulativeActivity) {
	
	def in(set : AlternativeCumulativeResource) { set.addActivity(act, cum) }
}

class AlternativeUnitUsage(act : Activity, cum : CumulativeActivity) {
	
	def in(set : AlternativeUnitResource) { set.addActivity(act, cum) }
}

