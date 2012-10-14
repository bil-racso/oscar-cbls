package oscar.cp.mem.pareto

import oscar.cp.mem.Hypervolume

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
	
	override def hypervolume(ref : Array[Int]) : Double = {
	
		val points : Array[Array[Double]] = paretoToArray
		
		/*for (i <- 0 until points.size) {
			points(i)(0) = ref(0) - points(i)(0)
			points(i)(1) = ref(1) - points(i)(1)
		}*/
		
		val h : Double = Hypervolume.simple2DHypervolume(points, Array(0, 0)) / (ref(0) * ref(1))	
		h
	}
	
	private def paretoToArray = set.map(p => p.toArray).toArray
	
	override def toString : String = "{" + set.mkString(", ") + "}"
}

object ParetoSet {
	def apply[S](nObjs : Int) = new ParetoSet[S](nObjs)
}