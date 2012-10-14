package oscar.cp.mem.pareto

class ParetoPoint[S](private val sol : S, val neighbourhood : Array[LinkedNode]) {
	
	def size = neighbourhood.size
	private val Size = 0 until size
	
	def solution : S = sol
	
	def apply(i : Int) : Int = neighbourhood(i).value
	
	def upperValue(d : Int) = {
		if (!neighbourhood(d).isLast) 
			neighbourhood(d).next.value
		else
			Int.MaxValue
	}
	
	def lowerValue(d : Int) = {
		if (!neighbourhood(d).isFirst) 
			neighbourhood(d).prev.value
		else
			Int.MinValue
	}

	def dominating(objs : Array[Int]) : Int = dominating0(objs, 0, ParetoPoint.NEITHER)
	private def dominating0(p : Array[Int], d : Int, s : Int) : Int = {
		
		if (d == size) s
		
		else if (this(d) < p(d)) {
			if (s == ParetoPoint.DOMINATING) ParetoPoint.NEITHER
			else dominating0(p, d+1, ParetoPoint.DOMINATED)
		}
			
		else if (this(d) > p(d)) {
			if (s == ParetoPoint.DOMINATED) ParetoPoint.NEITHER
			else dominating0(p, d+1, ParetoPoint.DOMINATING)
		} 
		
		else dominating0(p, d+1, s)
	}
}

object ParetoPoint {
	
	val NEITHER    = 0
	val DOMINATED  = -1
	val DOMINATING = 1
	
	def apply[S](s : S, n : Array[LinkedNode]) : ParetoPoint[S] = new ParetoPoint[S](s, n) 
}
