package oscar.cp.memScheduling

class ActivitiesRequirements[A <: Activity](activities: Array[A], requirements: Array[Int]) {
  
  def ofResources(resources: Array[UnitResource]) {
    for (i <- 0 until activities.length) {
    		activities(i) needs resources(requirements(i))
    }
  }

}

object ActivitiesRequirements {
	def apply[A <: Activity](activities : Array[A], requirements: Array[Int]) = new ActivitiesRequirements(activities, requirements)
}