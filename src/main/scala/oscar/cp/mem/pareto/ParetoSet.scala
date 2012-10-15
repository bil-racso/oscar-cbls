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
		
		val points = set.toArray
		val formatedPoints = points.map(p => Array((ref(0) - p(0))/ref(0).toDouble, (ref(1) - p(1))/ref(1).toDouble))
		
		val vol = Hypervolume.simple2DHypervolume(formatedPoints, Array(0,0))
		
		if (vol < 0) throw new Exception("Hypervolume is negative")
		
		vol
	}
	
	override def toString : String = "{" + set.mkString(", ") + "}"
}

object ParetoSet {
	def apply[S](nObjs : Int) = new ParetoSet[S](nObjs)
}