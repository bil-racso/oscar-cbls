package oscar.cp.scheduling;

import scala.collection.mutable.Set

import oscar.cp.modeling.CPScheduler
import oscar.cp.constraints.BoundedSweepCumulative

class ContainerResource(scheduler : CPScheduler, capa : Int, init : Int) extends Resource(scheduler) {

	// Initial stock must be lower or equal to the capacity
	assert(init <= capa)
	
	protected val activitiesSet : Set[CumulativeActivity] = Set()

	def activities = activitiesSet.toArray
	def capacity   = capa
	
	def addActivity(activity : Activity, height : Int) {		
		activitiesSet.add(CumulativeActivity(activity, id, height))
	}

	override def setup() {
		scheduler.add(new BoundedSweepCumulative(scheduler, activitiesSet.toArray, 0, capa, id))
	}
	
	def criticality = activities.map(_.maxDuration).sum
}

object ContainerResource {
	
	def apply(scheduler : CPScheduler, capa : Int) = new CumulativeResource(scheduler, capa)
}