package oscar.cp.mem

import oscar.cp.mem.ParetoFront.ParetoSet

import oscar.cp.mem.ParetoFront.ParetoPoint

abstract class MOLNS_Abstract(val nObjective : Int) {

	// Approximation of the pareto set
	val paretoSet : ParetoSet = ParetoSet(nObjective)
	
	var currentObjective : Int = 0
	var currentPoint     : ParetoPoint = null
	
	var restart = 0
	var objective = 0
	
	val nRestart = 1
	
	var status : Boolean = false
	
	/** Select the next point to process
	 */
	def selectPoint : ParetoPoint
	
	/** Select next objective to improve
	 */
	def selectObjective : Int
	
	def relax = {
		
		for (i <- 0 until nObjective) {
			
			if (i == currentObjective) {
				
				// fix to best value
				
			} else {
		
				// Relax best value
			}
		}
	}
	
	def frameWork = {
		
		// Relax the other objectives
		relax
		
		//Perform LNS relaxation 
		val newPoint = LNS(currentPoint.sol, currentObjective)
		
		if (newPoint != null) {
			
			// Add the point to the set
			paretoSet.add(newPoint)	
			// Process a new point
			currentPoint = selectPoint
			// Reset the number of considered objectives
			objective = 0
			
		} else {
			
			// Next objective
			currentObjective = selectObjective
			objective = objective + 1
			
			if (objective >= nObjective) {
				
				currentPoint = selectPoint
				objective = 0
			}
		}
	}
	
	def LNS(sol : MOSolution, objective : Int) : ParetoPoint
}