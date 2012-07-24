package oscar.cp.scheduling

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import scala.util.Random.nextInt
import scala.Math.ceil

object PartialOrderSchedule {
	
	val gapStruct1 = new GapStructure
	val gapStruct2 = new GapStructure
	
	var precedences : Set[Tuple2[Int, Int]] = Set()
	
	def divide(allTasks : Array[FixedActivity], tasks : Queue[Tuple2[Int, Int]], limit : Int) {
		
		if (!stopCriterion(tasks, limit)) {
			
			val limit1 = ceil(limit/2).toInt
			val limit2 = limit - limit1
			
			gapStruct1.reset(limit1)
			gapStruct2.reset(limit2)
			
			val tasks1 : Queue[Tuple2[Int, Int]] = Queue()
			val tasks2 : Queue[Tuple2[Int, Int]] = Queue()

			while (!tasks.isEmpty) {
				
				val t = tasks.head._1
				val c = tasks.head._2
				tasks.dequeue
				
				val gap1 = gapStruct1.getGap(allTasks(t).start)
				val gap2 = gapStruct2.getGap(allTasks(t).start)
				
				val (minQueue,minStruct,minGap,maxQueue,maxStruct,maxGap) = 
				if (gap1 < gap2) {
					(tasks1, gapStruct1, gap1, tasks2, gapStruct2, gap2)
				} else {			
					(tasks2, gapStruct2, gap2, tasks1, gapStruct1, gap1)
				}
				
				if (maxGap >= c) {
					
					maxStruct.add(allTasks(t).end, c)
					maxQueue.enqueue((t, c))
				} else {
					
					val r = c - maxGap
					
					maxStruct.add(allTasks(t).end, maxGap)
					maxQueue.enqueue((t, maxGap))
					minStruct.add(allTasks(t).end, r)
					minQueue.enqueue((t, r))
				}
			}
			
			if (!tasks1.isEmpty) {
				divide(allTasks, tasks1, limit1)	
			}
			
			if (!tasks2.isEmpty) {
				divide(allTasks, tasks2, limit2)
			}
				
		} else {
			
			while (tasks.size > 1)
				precedences.add((tasks.dequeue._1, tasks.head._1))
		}
	}
	
	def getPrecedences(allTasks : Array[FixedActivity], capacity : Array[Int]) : Array[Tuple2[Int, Int]] = {
		
		precedences.clear
		
		// Sort the tasks by start, inc and random as tie breaker
		val sortedTasks = allTasks.sortBy(t => (t.start, -t.inc, nextInt))
		
		for (m <- 0 until capacity.size) {
	
			val tasks : Queue[Tuple2[Int, Int]] = Queue()
			
			val filteredTasks = sortedTasks.filter(_.machine == m)
			
			for(i <- 0 until filteredTasks.size)
				tasks.enqueue((filteredTasks(i).id, filteredTasks(i).inc))
			
			divide(allTasks, tasks, capacity(m))
		}
		
		// Sort precedences in a topological order
		return precedences.toArray.sortBy(t => sortedTasks(t._1).start)
	}
	
	def stopCriterion(tasks : Queue[Tuple2[Int, Int]], capacity : Int) : Boolean = { 

		if (tasks.size <= 2) 
			return true
			
		val s = tasks.toArray.sortBy(_._2)
		
		if (s(0)._2 + s(1)._2 > capacity)
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
			
			while (!eventPointSeries.isEmpty && eventPointSeries.head._2 <= date)
				gap += eventPointSeries.dequeue()._1

			return gap
		}
		
		def add(end : Int, inc : Int) {
			
			gap -= inc
			eventPointSeries.enqueue((inc, end))
		}
	}
	
	def main(args: Array[String]) {
		
		// (id, s, e, i, m)
		val a = new FixedActivity(0, 0, 3, 3, 0)
		val b = new FixedActivity(1, 0, 2, 1, 0)
		val c = new FixedActivity(2, 2, 3, 2, 0)
		val d = new FixedActivity(3, 3, 6, 4, 0)
		val e = new FixedActivity(4, 5, 10, 1, 0)
		val f = new FixedActivity(5, 6, 7, 3, 0)
		val g = new FixedActivity(6, 7, 9, 2, 0)
		val h = new FixedActivity(7, 9, 10, 4, 0)
		val i = new FixedActivity(8, 10, 12, 1, 0)
		
		val tasks = Array(a, b, c, d, e, f, g, h, i)
		
		val capacity = 5
		
		val precedencesSet : Set[Tuple2[Int,Int]] = Set((0, 3), // A -> D
														(1, 2), // B -> C
														(2, 3), // C -> D
														(2, 4), // C -> E
														(3, 5), // D -> F
														(3, 7), // D -> H
														(4, 8), // E -> I
														(5, 7), // F -> H
														(5, 6), // F -> G
														(6, 7), // G -> H
														(7, 8)) // H -> I
														
		val tasksName = Array("A", "B", "C", "D", "E", "F", "G", "H", "I")
		
		val precedences = PartialOrderSchedule.getPrecedences(tasks, Array(capacity))
		
		for(p <- precedences)
			println(tasksName(p._1) + " -> " + tasksName(p._2))
	}
}