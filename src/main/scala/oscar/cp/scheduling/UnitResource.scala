package oscar.cp.scheduling

//import scala.collection.mutable.Set
import scala.collection.mutable.Map

import oscar.cp.modeling._
import oscar.cp.constraints.MaxSweepCumulative
import oscar.cp.core.CPVarInt
import java.security.InvalidParameterException

class UnitResource(scheduler : CPScheduler) extends Resource(scheduler) {

	//protected val activitiesSet : Set[Activity] = Set()
	protected val activitiesSet : Map[Activity, CumulativeActivity] = Map()

	//def activities = activitiesSet.toArray
	def activities = activitiesSet.values.toArray

	def addActivity(activity : Activity) {
		//activitiesSet.add(activity)
		activitiesSet += (activity ->CumulativeActivity(activity, id, 1))
	}

	override def setup() {
		//scheduler.add(unaryResource(activities))
		scheduler.add(cumulative(activities, id, max = 1))
	}
	
	def criticality = activities.map(_.maxDuration).sum
}

object UnitResource {

	def apply(scheduler : CPScheduler) = new UnitResource(scheduler)
}