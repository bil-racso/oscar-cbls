package oscar.cp.scheduling

import scala.collection.mutable.Set

import oscar.cp.modeling._
import oscar.cp.core.Constraint
import oscar.cp.constraints.MaxSweepCumulative

class UnaryResource(scheduler : CPScheduler) extends Resource(scheduler) {

	protected val activitiesSet : Set[CumulativeActivity] = Set()

	def activities = activitiesSet.toArray

	def addActivity(activity : Activity) {
		activitiesSet.add(CumulativeActivity(activity, id, 1))
	}

	override def setup() {
		val cons : Constraint = new MaxSweepCumulative(scheduler, activitiesSet.toArray, 1, id)
		scheduler.add(cons)
	}
	
	def criticality = activities.map(_.maxDuration).sum
}

object UnaryResource {

	def apply(scheduler : CPScheduler) = new UnaryResource(scheduler)
}