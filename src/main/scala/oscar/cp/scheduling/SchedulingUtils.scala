package oscar.cp.scheduling

import oscar.cp.modeling.CPModel
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPOutcome
import oscar.reversible.ReversibleSetIndexedArray
import oscar.reversible.ReversibleInt
import scala.util.continuations._
import scala.collection.JavaConverters._
import oscar.reversible.ReversibleBool


object SchedulingUtils extends CPModel {

	def setTimesSearch(cp : CPSolver, activities : Array[CumulativeActivity]) : Unit @suspendable = {
		
		// Non fixed activities
		val selectable = Array.tabulate(activities.size) { 
			i => if (activities(i).start.isBound()) 
					 new ReversibleBool(cp,false)
		  		 else 
		  			 new ReversibleBool(cp,true) 
		}

		val oldEST = Array.fill(activities.size)(new ReversibleInt(cp,-1))
		
		def updateSelectable() = {
			
			for (i <- 0 until activities.size) {	
				if (activities(i).start.isBound()) { 	
					selectable(i).value = false
					
				} else if (oldEST(i).value != activities(i).est()) {			
					selectable(i).value = true
				}
			}
		}
		
		def selectableIndices() = (0 until activities.size).filter(i => selectable(i).value)
		
		def allStartBounds() = activities.forall(i => i.start.isBound())

		while (!allStartBounds()) {
			
			// Get the smallest EST
			val (est,ect) = selectableIndices().map(i => (activities(i).est,activities(i).ect)).min
			
			// Select the activity with the smallest EST, ECT as tie breaker
			val x = selectableIndices().filter(i => activities(i).est == est && activities(i).ect == ect).first
				
			cp.branch {
			  	  
				cp.post(activities(x).start == est)
				oldEST(x).value = -1
				updateSelectable()
				
				if (selectableIndices().isEmpty && !allStartBounds()) cp.fail()
			} {

				selectable(x).value = false
				oldEST(x).value = est
				updateSelectable()
				
				if (selectableIndices().isEmpty && !allStartBounds()) cp.fail()
			}	
		}		
	}
}