package oscar.cp.scheduling

import scala.collection.mutable.Set

import oscar.cp.modeling.CPScheduler
import oscar.cp.constraints.MaxSweepCumulative

class CumulativeResource(scheduler : CPScheduler, capacity : Int) {
	
	val id : Int = 0
	
	val activitiesSet : Set[CumulativeActivity] = Set()
	
	def addActivity(activity : Activity, height : Int) {		
		activitiesSet.add(CumulativeActivity(activity, id, height))
	}

	def setup() {
		
		scheduler.add(new MaxSweepCumulative(scheduler, activitiesSet.toArray, capacity, id))
	}
}

object CumulativeResource {
	
	def apply(scheduler : CPScheduler, capacity : Int) = new CumulativeResource(scheduler, capacity)
}