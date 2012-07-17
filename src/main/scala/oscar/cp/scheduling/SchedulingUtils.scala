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
		val selectable = Array.tabulate(activities.size) { i => 
		  										if (activities(i).getStart().isBound()) new ReversibleBool(cp,false)
		  										else new ReversibleBool(cp,true) }

		val oldEST = Array.fill(activities.size)(new ReversibleInt(cp,-1))
		
		def updateSelectable() = {
		  for (i <- 0 until activities.size) {
		    if (activities(i).getStart.isBound()) { 
				selectable(i).value = false
		    } else if (oldEST(i).value != activities(i).getEST()) {
		         selectable(i).value = true
		    }
		  }
		}
		
		def selectableIndices() = (0 until activities.size).filter(i => selectable(i).value)
		
		def allStartBounds() = activities.forall(i => i.getStart().isBound())

		while (!allStartBounds()) {
		  // Get the smallest EST
		  val (est,ect) = selectableIndices().map(i => (activities(i).getEST,activities(i).getECT)).min
		  // Select the activity with the smallest EST, ECT as tie breaker
	      val x = selectableIndices().filter(i => activities(i).getEST == est && activities(i).getECT == ect).first
				
		  cp.branch {
			  	  //println("left")
				  cp.post(activities(x).getStart == est)
				  oldEST(x).value = -1
				  updateSelectable()
				  if (selectableIndices().isEmpty && !allStartBounds()) cp.fail()
		  } {
			  	  //println("right")
			  	  selectable(x).value = false
				  oldEST(x).value = est
				  updateSelectable()
				  if (selectableIndices().isEmpty && !allStartBounds()) cp.fail()
		  }	
		   
		  
		}		
	}
}