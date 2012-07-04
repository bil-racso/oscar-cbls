package oscar.visual

import oscar.cp.scheduling.CumulativeActivity

class VisualActivity2(activity : CumulativeActivity, color : Int) {
	
	def getStart() = activity.getEST()

	def getEnd() = activity.getLCT()
  
	def getHeight() = activity.getMaxResource()
	
	def getColor() = color
}

object VisualActivity2 {
	
	def apply(activity : CumulativeActivity) {
		
		val color = 0
		new VisualActivity2(activity, color)
	}
}