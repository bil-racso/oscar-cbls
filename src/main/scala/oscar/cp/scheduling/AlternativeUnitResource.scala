package oscar.cp.scheduling

import scala.collection.mutable.Map
import oscar.cp.modeling.CPScheduler
import oscar.cp.core.CPVarInt

class AlternativeUnitResource(scheduler : CPScheduler) {
	
	private var nResources = 0
	
	private val activitiesMap : Map[Activity, CumulativeActivity] = Map()
	private val resourcesMap  : Map[Int, UnitResource] = Map()

	def resources  : Array[UnitResource]       = resourcesMap.values.toArray
	def activities : Array[CumulativeActivity] = activitiesMap.values.toArray
	
	def resource(id : Int) = resourcesMap(id)
	
	def resourcesOf(act : Activity) : CPVarInt = activitiesMap(act).resource
	
	def addAlternative(resource : UnitResource) { 
		
		checkId(resource.id)
		resourcesMap += resource.id -> resource
		nResources += 1
	}
	
	def addActivity(act : Activity, cum : CumulativeActivity) {
		
		checkActivity(act)
		activitiesMap += act -> cum
		
		for (i <- cum.resource) {
			if (!resourcesMap.contains(i)) throw new IllegalArgumentException("id " +i+ " is not a declared resource.")
			resourcesMap(i).addActivity(cum)
		}
	}
	
	protected def checkId(id : Int) {
		if (resourcesMap.contains(id)) 
			throw new IllegalArgumentException("id " +id+ " is already used.")
	}
	
	protected def checkActivity(act : Activity) {
		if (activitiesMap.contains(act)) 
			throw new IllegalArgumentException("the activity is already assigned to this resource.")
	}
}

object AlternativeUnitResource {
	
	def apply(scheduler : CPScheduler) = new AlternativeUnitResource(scheduler)
	
	def apply(scheduler: CPScheduler, nResources : Int, capa : Int) = {
		
		val resourceSet = new AlternativeUnitResource(scheduler)
		
		for (i <- 0 until nResources)
			resourceSet addAlternative UnitResource(scheduler)
		
		resourceSet
	}
	
	def apply(resources: UnitResource*): AlternativeUnitResource = {
		val scheduler = resources.head.scheduler
		val alt = new AlternativeUnitResource(scheduler)
		resources foreach (alt.addAlternative(_))
		alt
	}
}
