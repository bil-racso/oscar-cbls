package oscar.cp.mem.pareto

class OldParetoSet {
/*
	// The queue of point
	private val set : Queue[ParetoPoint] = Queue()
	
	// Sorted list of objective values
	private val sortedPoint = Array.fill(nObjs)(new OrderedLinkedList)
	
	def currentPoint : ParetoPoint = {
		
		null
	}
	
	def currentSol : Array[Int] = {
		
		null
	}
	
	
	// Size of the queue
	def size = set.size
	def isEmpty = size == 0
	
	//Number of objectives
	def nObjs = n	
	private val Objs = 0 until n
	
	/** Return the first point of the queue (this point is then reinserted in the queue)
	 * 
	 */
	def nextPoint : ParetoPoint = {
		
		if (isEmpty) throw new NoSuchElementException("Pareto set is empty")
	
		set enqueue set.dequeue()
		currentPoint
	}
	
	/** This method add a new point in the approximation of the pareto set.
	 * 
	 *  First, we check the validity of the point in term of dimension.
	 * 
	 *  If this point is dominating some point of the approximation, those points 
	 *  are removed from the set.
	 */
	def insert(newPoint : ParetoPoint) = { 
		
		for (i <- Objs) {
			
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
	
	def buildNewPoint(values : Array[Int], sol : Array[Int]) : ParetoPoint = {
		
		val neighbourhood = Array.tabulate(nObjs)(i => sortedPoint(i) insert values(i))
		new ParetoPoint(sol, neighbourhood)
	}
	
	def hypervolume = 0.0
	
	private def delete(p : ParetoPoint) {
		
		for(i <- Objs) {
			sortedPoint(i) remove p.neighbourhood(i)
		}
	}*/
}