package oscar.cp.mem.pareto

import scala.collection.mutable.Queue
import oscar.cp.mem.paretoFront.OrderedLinkedList
import oscar.cp.mem.paretoFront.ParetoPoint
import oscar.cp.mem.pareto.ParetoPoint
import oscar.cp.mem.pareto.OrderedLinkedList

abstract class Pareto(n : Int) {
	
	protected val set : Queue[ParetoPoint] = Queue()
	
	protected val sortedPoint = Array.fill(nObjs)(new OrderedLinkedList)
	
	def currentPoint : ParetoPoint
	
	def currentSol : Array[Int]
	
	def size = set.size
	
	def isEmpty = size == 0
	
	def nObjs = n	
	
	def nextPoint : ParetoPoint
	
	def insert(objs : Array[Int], sol : Array[Int]) : Boolean
	
	def hypervolume : Double
}