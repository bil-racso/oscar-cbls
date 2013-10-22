package oscar.cp.memScheduling

import oscar.cp.scheduling._

class ActivitiesRequirements(activities: Array[Activity], requirements: Array[Int]) {
  
  def ofResources(resources: Array[UnitResource]) {
    for (i <- 0 until activities.length) {
    		activities(i) needs resources(requirements(i))
    }
  }
  
  def ofResources(resources: Array[CumulativeResource]) {
    // TODO
  }

}

object ActivitiesRequirements {
	def apply(activities : Array[Activity], requirements: Array[Int]) = new ActivitiesRequirements(activities, requirements)
}