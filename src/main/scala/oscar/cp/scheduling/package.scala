package oscar.cp

import oscar.cp.core.CPVarInt
import oscar.cp.modeling.CPScheduler
import oscar.cp.scheduling._

/**
 * @author Renaud Hartert : ren.hartert@gmail.com
 */
package object scheduling {
	
	// ImplicitVarInt
	implicit def var2ImplicitVarInt(v : CPVarInt)     = VarImplicitVarInt(v)
	implicit def array2ImplicitVarInt(a : Array[Int]) = ArrayImplicitVarInt(a)
	implicit def range2ImplicitVarInt(r : Range)      = RangeImplicitVarInt(r)
	implicit def int2ImplicitVarInt(i : Int)          = IntImplicitVarInt(i)
}
