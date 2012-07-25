package oscar.visual

import oscar.cp.scheduling.CumulativeActivity

class VisualActivity(activity : CumulativeActivity) {
	
	def start = activity.est

	def end = activity.lct
  
	def resource = activity.maxResource
	
	def machine = activity.machine.getMin
}

object VisualActivity {
	
	def apply(activity : CumulativeActivity) = { new VisualActivity(activity) }
}