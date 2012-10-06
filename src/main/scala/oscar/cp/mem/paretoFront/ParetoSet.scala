package oscar.cp.mem.paretoFront

import scala.collection.mutable.Queue

class ParetoSet(nDim : Int) {
	
	def nDimension = nDim	
	private val Dimensions = 0 until nDimension
	
	val set : Queue[ParetoPoint] = Queue()
	val sortedPoint = Array.fill(nDimension)(new OrderedLinkedList)
	
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
	def add(newPoint : ParetoPoint) = { 
		
		checkPoint(newPoint)
		
		for (i <- Dimensions) {
			
			val p = set.dequeue() 
			val dom = p dominating newPoint
			
			if (dom == ParetoPoint.NEITHER) {		
				set enqueue p
				set enqueue newPoint
			}
			
			else if (dom == ParetoPoint.DOMINATED) {
				set enqueue newPoint
				delete(p)	
			}	
				
			else {
				set enqueue p
				delete(newPoint)
			}
		}
	}

	private def checkPoint(p : ParetoPoint) : Boolean = {
		
		if (p.size != nDimension) 
			throw new Exception("The dimension of the point does not fit the dimension of the set")
		
		true
	}
	
	def buildNewPoint(values : Array[Int], sol : Array[Int]) : ParetoPoint = {
		
		val neighbourhood = Array.tabulate(nDimension)(i => sortedPoint(i) insert values(i))
		new ParetoPoint(sol, neighbourhood)
	}
	
	def delete(p : ParetoPoint) {
		
		for(i <- Dimensions) {
			sortedPoint(i) remove p.neighbourhood(i)
		}
	}
}

object ParetoSet {
	
	def apply(nDimension : Int) = new ParetoSet(nDimension)
}