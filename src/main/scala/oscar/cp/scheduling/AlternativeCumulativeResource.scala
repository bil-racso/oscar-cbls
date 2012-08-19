package oscar.cp.scheduling

import scala.collection.mutable.Map
import oscar.cp.modeling.CPScheduler
import oscar.cp.core.CPVarInt


class AlternativeCumulativeResource(scheduler : CPScheduler) {
	
	private var nResources = 0
	
	private val activitiesMap : Map[Activity, CumulativeActivity] = Map()
	private val resourcesMap  : Map[Int, CumulativeResource] = Map()

	def resources  : Array[CumulativeResource] = resourcesMap.values.toArray
	def activities : Array[CumulativeActivity] = activitiesMap.values.toArray
	
	def capacities : Array[Int] = resourcesMap.values.toArray.map(_.capacity)
	
	def capacity(id : Int) = resourcesMap(id).capacity
	def resource(id : Int) = resourcesMap(id)
	
	def resourcesOf(act : Activity) : CPVarInt = activitiesMap(act).resource
	def heightOf(act : Activity)    : CPVarInt = activitiesMap(act).height
	
	def addAlternative(resource : CumulativeResource) { 
		
		checkId(resource.id)
		resourcesMap += resource.id -> resource
		nResources += 1
	}
	
	def addActivity(act : Activity, cum : CumulativeActivity) {
		
		checkActivity(act)
		activitiesMap += act -> cum
		
		for (i <- cum.resource) {
			if (!resourcesMap.contains(i)) throw new IllegalArgumentException("id " +i+ " is not a reference resource.")
			resourcesMap(i).addActivity(act, cum)
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

object AlternativeCumulativeResource {
	
	def apply(scheduler : CPScheduler) = new AlternativeCumulativeResource(scheduler)
	
	def apply(scheduler: CPScheduler, nResources : Int, capa : Int) = {
		
		val resourceSet = new AlternativeCumulativeResource(scheduler)
		
		for (i <- 0 until nResources)
			resourceSet addAlternative MaxResource(scheduler, capa)
		
		resourceSet
	}
	
	def apply(resources: CumulativeResource*): AlternativeCumulativeResource = {
		val scheduler = resources.head.scheduler
		val alt = new AlternativeCumulativeResource(scheduler)
		resources foreach (alt.addAlternative(_))
		alt
	}
}