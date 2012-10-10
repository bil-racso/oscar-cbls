package oscar.cp.mem.pareto

class ParetoSet[S](n : Int) extends Pareto[S](n) {
	
	override def currentPoint : ParetoPoint[S] = {
		
		// Checks if the list is empty
		if (set.isEmpty) throw new NoSuchElementException("set is empty")		
		// First point of the set
		else set.head
	}
	
	override def nextPoint() : ParetoPoint[S] = {
		
		set.enqueue(set.dequeue())
		currentPoint
	}
	
	def removeCurrentPoint() : ParetoPoint[S] = {
		
		// Checks if the list is empty
		if (set.isEmpty) throw new NoSuchElementException("set is empty")
		// Remove the point from the set
		else set.dequeue()
	}
	
	override def hypervolume(ref : Array[Int]) : Double = 0
}

object ParetoSet {
	def apply[S](nObjs : Int) = new ParetoSet[S](nObjs)
}