package oscar.cp.scheduling

import oscar.cp.modeling.CPModel
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPOutcome

import oscar.reversible.ReversibleSetIndexedArray
import oscar.reversible.ReversibleInt

object SchedulingUtils extends CPModel {

	def setTimesSearch(cp : CPSolver, activities : Array[Activity]) {
		
		// Non fixed activities
		val selectable    = new ReversibleSetIndexedArray(cp, 0, activities.size-1, false)
		// Activities waiting to be reassigned
		val notSelectable = new ReversibleSetIndexedArray(cp, 0, activities.size-1, true)
		// Previous EST of the notSelectable activities
		var oldEST : Array[ReversibleInt] = new Array(activities.size)
		
		while(!allBounds(activities.map(_.getStart))) {
			
			// Remove fixed activities
			for (i <- 0 until activities.size; if(activities(i).getStart.isBound)) 
				selectable.removeValue(i)
				
			// Check modifications
			if (notSelectable.getSize > 0) {
				for (v <- notSelectable.getValues; if (oldEST(v).value < activities(v).getEST)) {
					selectable.insert(v)
					notSelectable.removeValue(v)	
				}
			}
			
			// If there is some selectable activities
			if (!selectable.isEmpty) {
					
				val selActivities = selectable.getValues
				
				// Get the smallest EST
				val v = selActivities.map(activities(_).getEST).min
				// Select the activity with the smallest EST
				// TODO : use ECT as tie breaker
				val x = selActivities.filter(activities(_).getEST == v).first
				
				// TODO : this failure is not considered as a backtrack 
				if (cp.post(activities(x).getStart == v) == CPOutcome.Failure) {
					
					oldEST(x).setValue(v)
					selectable.removeValue(x)
					notSelectable.insert(x)
				}
				
				cp.branch {
					
					cp.post(activities(x).getStart == v)
				}{
					oldEST(x).setValue(v)
					selectable.removeValue(x)
					notSelectable.insert(x)
				}

			} else {
				
				cp.fail
			}
		}
	}
}