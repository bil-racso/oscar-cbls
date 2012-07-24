package oscar.cp.scheduling

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Queue
import scala.util.Random.nextInt
import scala.Math.ceil

object PartialOrderSchedule {
	
	val gapStruct1 = new GapStructure
	val gapStruct2 = new GapStructure
	
	var precedences : Queue[Tuple2[Int, Int]] = Queue()
	
	def divide(tasks : Array[FixedActivity], limit : Int) {
		
		if (!stopCriterion(tasks, limit)) {
			
			val limit1 = ceil(limit/2).toInt
			val limit2 = limit - limit1
			
			gapStruct1.reset(limit1)
			gapStruct2.reset(limit2)
			
			val tasks1 : Queue[Tuple2[Int, Int]] = Queue()
			val tasks2 : Queue[Tuple2[Int, Int]] = Queue()

			for (i <- 0 until tasks.size) {
				
				val gap1 = gapStruct1.getGap(tasks(i).start)
				val gap2 = gapStruct2.getGap(tasks(i).start)
				
				val (minQueue,minStruct,minGap,maxQueue,maxStruct,maxGap) = 
				if (gap1 < gap2) {
					(tasks1, gapStruct1, gap1, tasks2, gapStruct2, gap2)
				} else {			
					(tasks2, gapStruct2, gap2, tasks1, gapStruct1, gap1)
				}
				
				if (maxGap >= tasks(i).inc) {
					
					maxStruct.add(tasks(i).end, tasks(i).inc)
					maxQueue.enqueue((i, tasks(i).inc))
				} else {
					
					val r = tasks(i).inc - maxGap
					
					maxStruct.add(tasks(i).end, maxGap)
					maxQueue.enqueue((i, maxGap))
					minStruct.add(tasks(i).end, r)
					minQueue.enqueue((i, r))
				}
			}
			
			if (!tasks1.isEmpty) {
				divide(tasks1.toArray.map(t => tasks(t._1)), limit1)	
			}
			
			if (!tasks2.isEmpty) {
				divide(tasks2.toArray.map(t => tasks(t._1)), limit2)	
			}
				
		} else {
			
			for (i <- 0 until tasks.size-1) 
				precedences.enqueue((tasks(i).id, tasks(i+1).id))
		}
	}
	
	def getPrecedences(tasks : Array[FixedActivity], capacity : Array[Int]) : Queue[Tuple2[Int, Int]] = {
		
		precedences.clear
		
		// Sort the tasks by start, inc and random as tie breaker
		val sortedTasks = tasks.sortBy(t => (t.start, t.inc, nextInt))
		
		for (m <- 0 until capacity.size)
			divide(sortedTasks.filter(_.machine == m), capacity(m))
		
		// Sort precedences in a topological order
		precedences.sortBy(t => sortedTasks(t._1).start)
		
		return precedences
	}
	
	def stopCriterion(tasks : Array[FixedActivity], capacity : Int) : Boolean = { 

		if (tasks.size <= 2) 
			return true
			
		val s = tasks.sortBy(_.inc)
		
		if (s(0).inc + s(1).inc <= capacity)
			return true
			
		return false
	}
	
	class GapStructure {
		
		val eventPointSeries = new PriorityQueue[Tuple2[Int, Int]]()(new Ordering[Tuple2[Int, Int]] { def compare(a : Tuple2[Int, Int], b : Tuple2[Int, Int]) = if (b._2 > a._2) {1} else if (b._2 == a._2) {0} else {-1} })
		var gap = 0
		
		def reset(limit : Int) {
			
			eventPointSeries.clear()
			gap = limit
		}
		
		def getGap(date : Int) : Int = {
			
			while (eventPointSeries.head._2 <= date)
				gap += eventPointSeries.dequeue()._1
			
			return gap
		}
		
		def add(end : Int, inc : Int) {
			
			gap -= inc
			eventPointSeries.enqueue((inc, end))
		}
	}
}