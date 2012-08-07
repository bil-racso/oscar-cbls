package oscar.cp

import oscar.cp.core.CPVarInt
import oscar.cp.modeling.CPScheduler

/**
 * @author Renaud Hartert : ren.hartert@gmail.com
 */
package object scheduling {
	
	// Precedences
	implicit def activityPrecedence2Constraint(ap : ActivityPrecedence) = ap.withDelay(0)
	
	// Capacities
	implicit def int2Capacity(i : Int) = IntCapacity(i)
	implicit def range2Capacity(r : Range) = RangeCapacity(r)
	implicit def variable2Capacity(v : CPVarInt) = VariableCapacity(v)
}
