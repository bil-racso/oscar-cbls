package oscar.cp.scheduling

import scala.collection.mutable.Map

import oscar.cp.modeling._
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.core.CPVarInt
import java.security.InvalidParameterException

class CumulativeResource(scheduler : CPScheduler, capa : Int, max : Boolean) extends Resource(scheduler) {
	
	protected val activitiesSet : Map[Activity, CumulativeActivity] = Map()
	
	def activities = activitiesSet.values.toArray
	def capacity   = capa
	
	def criticality = activities.map(_.maxDuration).sum
	
	def heightOf(act : Activity) : CPVarInt = activitiesSet(act).height
	
	override def setup() {
		if (max)
			scheduler.add(cumulative(activities, id, max = capa))
		else
			scheduler.add(cumulative(activities, id, min = capa))
	}
	
	// Adding an activity
	def addActivity(activity : Activity, height : CPVarInt) { 
		addActivity(activity, CumulativeActivity(activity, id, height)) 
	}
	
	def addProdConsActivity(activity : Activity, height : CPVarInt, atEnd : Boolean) { 
		addActivity(activity, ProdConsActivity.apply(activity, id, height, atEnd)) 
	}
	
	def addActivity(act : Activity, cum : CumulativeActivity) {
		if (activitiesSet.contains(act)) 
			throw new InvalidParameterException("The activity is already scheduled on this resource.")
		else 
			activitiesSet += (act -> cum)
	}
}

object CumulativeResource {
	
	def apply(scheduler : CPScheduler, capa : Int, max : Boolean = true) = new CumulativeResource(scheduler, capa, max)
}
