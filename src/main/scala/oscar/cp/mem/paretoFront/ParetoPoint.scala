package oscar.cp.mem.paretoFront

import oscar.cp.constraints.CPObjective
import oscar.cp.core.CPVarInt
import oscar.cp.mem.MOSolution

class ParetoPoint(private val sol : Array[CPVarInt], private val neighbourhood : Array[LinkedNode]) {
	
	def size = sol.size
	private val Size = 0 until size
	
	def apply(i : Int) : Int = neighbourhood(i).value

	/** A point p1 dominates an other point p2 if for all i, p1(i) >= p2(i) and
	 *  there is a i for which p1(i) > p2(i). 
	 */
	def isDominating(p : ParetoPoint) : Boolean = {
		
		// Check sizes
		if (size != p.size) false
		
		var dom = false
		
		for (i <- Size) {
			
			if (p(i) > this(i)) false
				
			if (p(i) < this(i)) dom = true
		}	
		
		dom
	}
}
