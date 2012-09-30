package oscar.cp.multiobjective

class ParetoPoint(values : Array[Int]) {

	val size = values.size
	val Size = 0 until size
	
	val sol : MOSolution = null
	
	def apply(i : Int) = values(i)

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

object ParetoPoint {
	
	def apply(size : Int) : ParetoPoint = new ParetoPoint(new Array[Int](size))
}