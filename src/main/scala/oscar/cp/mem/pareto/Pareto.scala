package oscar.cp.mem.pareto

import scala.collection.mutable.Queue

abstract class Pareto[S](n : Int) {
	
	// Priority set of non-dominated points
	protected val set : Queue[ParetoPoint[S]] = Queue()
	// Allows a O(1) query of the previous or next objective value of each point
	protected val sortedPoint = Array.fill(nObjs)(new OrderedLinkedList)
	
	/** @return the solution of the current pareto point
	 */
	def currentSol : S = currentPoint.solution
	
	/** @return the number of points in the set
	 */
	def size = set.size
	
	/** @return true if the set is empty, false otherwise
	 */
	def isEmpty = size == 0
	
	/** @return the size of the objective space
	 */
	def nObjs = n	
	
	/** Builds and try to insert the point corresponding to the objective values and the solution
	 *  sol.
	 *  
	 *  Points of the set that are dominated by this new point are removed from the set.
	 *  
	 *  @return true if the point is inserted, false otherwise
	 */
	def insert(objs : Array[Int], sol : S) : Boolean = {
		
		if (cleaning(objs)) {			
			val point = ParetoPoint[S](sol, computeNeighbourhood(objs))
			set.enqueue(point)
			true
		} 
		else false
	}
	
	/** Removes the dominated points from the set. 
	 * 
	 *  @return true if the new point is not dominated, false otherwise
	 */
	private def cleaning(objs : Array[Int]) : Boolean = cleaning0(objs, true, 0)
	private def cleaning0(objs : Array[Int], insert : Boolean, i : Int) : Boolean = {
		
		if (i == size) 
			insert	
		else {
			val dom = currentPoint.dominating(objs)
				
			// The point is dominated
			if (dom == ParetoPoint.DOMINATING) {
				nextPoint()
				cleaning0(objs, false, i+1)
			}				
			// The point is neither dominated or dominating
			else if (dom == ParetoPoint.NEITHER) { 
				nextPoint()
				cleaning0(objs, insert, i+1)
			}		
			// The point is dominating
			else if (dom == ParetoPoint.DOMINATED) {
				removeCurrentPoint()
				cleaning0(objs, insert, i)
			}
			else throw new RuntimeException("unknown dominance relation")
		}
	}
	
	/** @return pointers to the linked nodes 
	 */
	private def computeNeighbourhood(values : Array[Int]) : Array[LinkedNode] = {
		
		// Checks if the number of values match 
		if (values.size != nObjs) throw new IllegalArgumentException("number of values must be " + nObjs)
		// Pointer to the nodes in the OrderedLikedLists
		else Array.tabulate(nObjs)(i => sortedPoint(i) insert values(i))
	}
	
	// Abstract functions
	// ------------------------------
	
	/** Removes the current point from the set.
	 * 
	 *  @return the removed point
	 * 
	 */
	def removeCurrentPoint() : ParetoPoint[S]
	
	/** @return the currently selected point
	 */
	def currentPoint : ParetoPoint[S]
	
	/** Changes the current point with the next one.
	 * 
	 *  @return the next point
	 */
	def nextPoint() : ParetoPoint[S]
	
	/** @return the hypervolume measure of the set
	 */
	def hypervolume(ref : Array[Int]) : Double
}