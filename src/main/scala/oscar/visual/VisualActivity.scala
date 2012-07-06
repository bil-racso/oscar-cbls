package oscar.visual

import oscar.cp.scheduling.CumulativeActivity

class VisualActivity(activity : CumulativeActivity, col : Int) {
	
	def start = activity.getEST()

	def end = activity.getLCT()
  
	def resource = activity.getMaxResource()
	
	def color = col
}

object VisualActivity {
	
	def apply(activity : CumulativeActivity) = { new VisualActivity(activity, 0) }
}