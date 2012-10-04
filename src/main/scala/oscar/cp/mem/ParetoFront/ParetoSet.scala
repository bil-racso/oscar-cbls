package oscar.cp.mem.ParetoFront

import scala.collection.mutable.Queue

class ParetoSet(nDimension : Int) {
	
	val set : Queue[ParetoPoint] = Queue()
	
	def size = set.size

	def nextPoint : ParetoPoint = {
		val p = set.dequeue()
		set enqueue p
		p
	}
	
	/** This method add a new point in the approximation of the pareto set.
	 * 
	 *  First, we check the validity of the point in term of dimension.
	 * 
	 *  If this point is dominating some point of the approximation, those points 
	 *  are removed from the set.
	 */
	def add(point : ParetoPoint) = { 
		
		checkPoint(point)
		
		var b = false
		
		for (i <- 0 until set.size) {
			
			val p = set.dequeue() 
			
			if (p isDominating point)
				set enqueue p
			
			else {
				
				b = true
				
				if(!(point isDominating p)) 
					set enqueue p
			}
		}
		
		if (b) set enqueue point
	}
	
	/** True if the point is checked
	 *  
	 *  Else, throw an exception
	 */
	def checkPoint(p : ParetoPoint) : Boolean = {
		
		if (p.size != nDimension) 
			throw new Exception("The dimension of the point does not fit the dimension of the set")
		
		true
	}

}

object ParetoSet {
	
	def apply(nDimension : Int) = new ParetoSet(nDimension)
}