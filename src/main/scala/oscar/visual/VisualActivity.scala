package oscar.visual

import oscar.cp.scheduling.CumulativeActivity

class VisualActivity(activity : CumulativeActivity) {
	
	def start = activity.getEST()

	def end = activity.getLCT()
  
	def resource = activity.getMaxResource()
	
	def machine = activity.getMachines.getMin
}

object VisualActivity {
	
	def apply(activity : CumulativeActivity) = { new VisualActivity(activity) }
}