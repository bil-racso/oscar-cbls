package oscar.cp.scheduling

import scala.collection.mutable.Map

import oscar.cp.modeling._
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.core.CPVarInt
import java.security.InvalidParameterException

class CumulativeResourceSet(scheduler : CPScheduler, nResources : Int, capas : Array[Int]) extends Resource(scheduler) {
	
	protected val activitiesSet : Map[Activity, CumulativeActivity] = Map()
	
	def activities = activitiesSet.values.toArray
	def capacities = capas
	
	def addActivity(activity : Activity, resources : Array[Int], height : Int) { addActivity(activity, CumulativeActivity(activity, CPVarInt(scheduler, resources), height)) }
	
	def addActivity(activity : Activity, resources : Array[Int], height : Range) { addActivity(activity, CumulativeActivity(activity, CPVarInt(scheduler, resources), height)) }
	
	private def addActivity(act : Activity, cum : CumulativeActivity) {
		if (activitiesSet.contains(act)) 
			throw new InvalidParameterException("The activity is already scheduled on this resource.")
		else 
			activitiesSet += (act -> cum)
	}
	
	def resourcesOf(act : Activity) : CPVarInt = activitiesSet(act).resource
	def heightOf(act : Activity)    : CPVarInt = activitiesSet(act).height
	
	override def setup() {
		val acts = activities
		for (i <- 0 until nResources)
			scheduler.add(cumulative(acts, i, max = capas(i)))
	}
	
	def criticality = activities.map(_.maxDuration).sum
}

object CumulativeResourceSet {
	
	def apply(scheduler : CPScheduler, nResources : Int, capas : Array[Int]) = new CumulativeResourceSet(scheduler, nResources, capas)
	
	def apply(scheduler : CPScheduler, nResources : Int, capa : Int) = new CumulativeResourceSet(scheduler, nResources, Array.fill(nResources)(capa))
}