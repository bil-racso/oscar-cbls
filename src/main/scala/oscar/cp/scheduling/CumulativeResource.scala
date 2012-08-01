package oscar.cp.scheduling

import scala.collection.mutable.Set

import oscar.cp.modeling.CPScheduler
import oscar.cp.constraints.MaxSweepCumulative

class CumulativeResource(scheduler : CPScheduler, capa : Int) extends Resource(scheduler) {
	
	protected val activitiesSet : Set[CumulativeActivity] = Set()
	
	def activities = activitiesSet.toArray
	def capacity   = capa
	
	def addActivity(activity : Activity, height : Int) {		
		activitiesSet.add(CumulativeActivity(activity, id, height))
	}

	override def setup() {
		scheduler.add(new MaxSweepCumulative(scheduler, activitiesSet.toArray, capa, id))
	}
	
	def criticality = activities.map(_.maxDuration).sum
}

object CumulativeResource {
	
	def apply(scheduler : CPScheduler, capa : Int) = new CumulativeResource(scheduler, capa)
}