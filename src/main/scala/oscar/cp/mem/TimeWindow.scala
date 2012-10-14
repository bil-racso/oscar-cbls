package oscar.cp.mem

import oscar.cp.core._
import scala.math.max

class TimeWindow(cp : Store, previous : CPVarInt, arrival : CPVarInt, departure : Array[CPVarInt], dist : Array[Int], twStart : Int = -1) extends Constraint(cp, "TimeWindow") {

	override def setup(l : CPPropagStrength) : CPOutcome = {

		val oc = propagate()
		
		if (oc == CPOutcome.Suspend) {
			
			if (!previous.isBound) previous.callPropagateWhenDomainChanges(this)
			
			for(i <- 0 until departure.size)
				if (!departure(i).isBound) departure(i).callPropagateWhenBoundsChange(this)
        }
        
        return oc   
	}
	
	override def propagate() : CPOutcome = {
		
		val times = Array.fill(departure.size)(Int.MaxValue)
		
		for (i <- previous.min to previous.max; if (previous.hasValue(i)))
			times(i) = departure(i).min + dist(i)		
		
		val min = times.min
		
		if (twStart != -1) {
			arrival.updateMin(max(twStart, min))
		}
		else {
			arrival.updateMin(min)
		}
	}
}

