package oscar.cp

import oscar.cp.core.CPVarInt
import oscar.cp.modeling.CPScheduler

/**
 * @author Renaud Hartert : ren.hartert@gmail.com
 */
package object scheduling {
	
	// Precedences
	implicit def activityPrecedence2Constraint(ap : ActivityPrecedence) = ap.withDelay(0)
	
	// ImplicitVarInt
	implicit def var2ImplicitVarInt(v : CPVarInt)     = VarImplicitVarInt(v)
	implicit def array2ImplicitVarInt(a : Array[Int]) = ArrayImplicitVarInt(a)
	implicit def range2ImplicitVarInt(r : Range)      = RangeImplicitVarInt(r)
	implicit def int2ImplicitVarInt(i : Int)          = IntImplicitVarInt(i)
}
