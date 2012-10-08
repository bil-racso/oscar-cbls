package oscar.cp.mem.pareto

import oscar.cp.mem.paretoFront.LinkedNode
import oscar.cp.mem.pareto.LinkedNode

class ParetoPoint(private val sol : Array[Int], val neighbourhood : Array[LinkedNode]) {
	
	def size = sol.size
	private val Size = 0 until size
	
	def solution = sol
	
	def apply(i : Int) : Int = neighbourhood(i).value

	def dominating(p : ParetoPoint) : Int = dominating0(p, 0, ParetoPoint.NEITHER)
	
	private def dominating0(p : ParetoPoint, d : Int, s : Int) : Int = {
		
		if (d > size) s
		
		else if (this(d) < p(d)) {
			if (s == ParetoPoint.DOMINATING)
				ParetoPoint.NEITHER
			else
				dominating0(p, d+1, ParetoPoint.DOMINATED)
		}
			
		else if (this(d) > p(d)) {
			if (s == ParetoPoint.DOMINATED) 
				ParetoPoint.NEITHER
			else 
				dominating0(p, d+1, ParetoPoint.DOMINATING)
		} 
		
		else dominating0(p, d+1, s)
	}
	
	def nextValue(d : Int) = {
		if (!neighbourhood(d).isLast) 
			neighbourhood(d).next.value
		else
			Int.MaxValue
	}
}

object ParetoPoint {
	
	val NEITHER    = 0
	val DOMINATED  = -1
	val DOMINATING = 1
}
