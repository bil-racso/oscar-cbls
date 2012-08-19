package oscar.cp.scheduling

import scala.collection.mutable.Map

import oscar.cp.modeling._
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.core.CPVarInt
import java.security.InvalidParameterException

class CumulativeResource(scheduler : CPScheduler, maxCapa : Int = Int.MaxValue, minCapa : Int = Int.MinValue, n : String = null) extends Resource(scheduler, n = n) {
	
	protected val activitiesSet : Map[Activity, CumulativeActivity] = Map()
	
	def activities = activitiesSet.values.toArray
	def capacity   = maxCapa
	
	def criticality = activities.map(_.minDuration).sum
	
	def heightOf(act : Activity) : CPVarInt = activitiesSet(act).height
	
	override def setup() {
		
		if (minCapa != Int.MinValue && maxCapa != Int.MaxValue) {
			scheduler.add(cumulative(activities, id, max = maxCapa, min = minCapa))
		}
		else if (minCapa == Int.MinValue && maxCapa != Int.MaxValue) {
			scheduler.add(cumulative(activities, id, max = maxCapa))
		}
		else if (minCapa != Int.MinValue && maxCapa == Int.MaxValue) {
			scheduler.add(cumulative(activities, id, min = minCapa))
		}
		else throw new InvalidParameterException("cumulative constraint bounded between -Infinity and Infinity")
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

object MaxResource {
	
	def apply(scheduler : CPScheduler, capa : Int, name : String = null) = new CumulativeResource(scheduler, maxCapa = capa, n = name)
}

object MinResource {
	
	def apply(scheduler : CPScheduler, capa : Int, name : String = null) = new CumulativeResource(scheduler, minCapa = capa, n = name)
}

object ContainerResource {
	
	def apply(scheduler : CPScheduler, maxCapa : Int, name : String = null) = new CumulativeResource(scheduler, maxCapa, 0, name)
}

object BoundedResource {
	
	def apply(scheduler : CPScheduler, maxCapa : Int, minCapa : Int, name : String = null) = new CumulativeResource(scheduler,  maxCapa, minCapa, name)
}
