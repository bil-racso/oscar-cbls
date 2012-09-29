package oscar.cp.multiobjective

import scala.collection.mutable.Queue

class ParetoSet(nDimension : Int) {

	
	/* DominatingDirection must be an array of position self-aware heap. Indeed,
	 * each solution of the pareto set must know its position in each heap in order
	 * to be removed if necessary. In the other way, solution should be able to be 
	 * added. Finally, we don't know the size of the heap. This imply to use a dynamic
	 * array.
	 *
	 */
	val dominatingDirection : Array[SelfHeap] = null
	
	val set : Queue[ParetoPoint] = null
	

	def nextPoint : ParetoPoint = {
		val p = set.dequeue
		set enqueue p
		p
	}
	
	//TODO
	/** This method add a new point in the approximation of the pareto set.
	 * 
	 *  First, we check the validity of the point in term of dimension.
	 * 
	 *  If this point is dominating some point of the approximation, those points 
	 *  are removed from the set.
	 */
	def add(p : ParetoPoint) = { 
		
		checkPoint(p)
		set.enqueue(p)
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