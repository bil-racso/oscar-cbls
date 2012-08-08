package oscar.cp

import oscar.cp.core.CPVarInt
import oscar.cp.modeling.CPScheduler

/**
 * @author Renaud Hartert : ren.hartert@gmail.com
 */
package object scheduling {
	
	// Precedences
	implicit def activityPrecedence2Constraint(ap : ActivityPrecedence) = ap.withDelay(0)
	
	// RichVarInt
	implicit def var2RichVarInt(v : CPVarInt)     = VarRichVarInt(v)
	implicit def array2RichVarInt(a : Array[Int]) = ArrayRichVarInt(a)
	implicit def range2RichVarInt(r : Range)      = RangeRichVarInt(r)
	implicit def int2RichVarInt(i : Int)          = IntRichVarInt(i)
}
