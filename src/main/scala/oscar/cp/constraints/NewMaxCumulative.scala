package oscar.cp.constraints

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.Set
import scala.collection.mutable.Queue

import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling.CPModel

class NewMaxCumulative(cp: CPSolver, allTasks : Array[CumulativeActivity], limit : Int, r : Int)  extends Constraint(allTasks(0).getMachines.getStore(), "NewMaxCumulative") {
	
	val lToRTasks : Array[CumulativeActivity] = allTasks.filter(_.getMachines.hasValue(r))
	val rToLTasks : Array[CumulativeActivity] = lToRTasks.map(new MirrorActivity(_))
	
	val nTasks = lToRTasks.size
	val Tasks  = 0 until nTasks
	
	val hEvents   = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = if (b.date > a.date) {1} else if (b.date == a.date) {0} else {-1} })
	
	val hCheck    = new PriorityQueue[Tuple2[Int, Int]]()(new Ordering[Tuple2[Int, Int]] { def compare(a : Tuple2[Int, Int], b : Tuple2[Int, Int]) = if (b._1 < a._1) {1} else if (b._1 == a._1) {0} else {-1} })
	val hCheckSet = Set[Int]()
	
	val hConflict = new PriorityQueue[Tuple2[Int, Int]]()(new Ordering[Tuple2[Int, Int]] { def compare(a : Tuple2[Int, Int], b : Tuple2[Int, Int]) = if (b._1 > a._1) {1} else if (b._1 == a._1) {0} else {-1} })
	val hConflictSet = Set[Int]()
	
	val newActiveTasks : Queue[Int] = Queue()
	
	val evup = new Array[Boolean](nTasks)
	val mins = new Array[Int](nTasks)
	
	var delta    = 0
	var deltaBis = 0	
	var gap	     = 0

	override def setup(l: CPPropagStrength) : CPOutcome = {
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- Tasks) {
    		
        		if (!lToRTasks(i).getStart.isBound) lToRTasks(i).getStart.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).getDur.isBound) lToRTasks(i).getDur.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).getEnd.isBound) lToRTasks(i).getEnd.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).getResource.isBound) lToRTasks(i).getResource.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).getMachines.isBound) lToRTasks(i).getMachines.callPropagateWhenDomainChanges(this)
        	}
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
			
		// Left to Right sweep
		if (sweepAlgorithm(lToRTasks) == CPOutcome.Failure) return CPOutcome.Failure
			
		// Right to Left sweep
		if (sweepAlgorithm(rToLTasks) == CPOutcome.Failure) return CPOutcome.Failure
        
		return CPOutcome.Suspend
	}
	
	def initDataStructures(tasks : Array[CumulativeActivity]) {
		
		hEvents.clear
		generateEvents(tasks)
		
		hCheck.clear
		hCheckSet.clear
		hConflict.clear
		hConflictSet.clear
		newActiveTasks.clear
		
		delta = hEvents.head.date
		deltaBis = hEvents.head.date
		gap = limit
	}
	
	def generateEvents(tasks : Array[CumulativeActivity]) {
		
		for (i <- Tasks; if (tasks(i).getMachines.isBoundTo(r))) {
			
			if (tasks(i).getLST < tasks(i).getECT) {
				
				// Generates events
				hEvents enqueue new Event(EventType.SCP, i, tasks(i).getLST, -tasks(i).getMinResource)
				hEvents enqueue new Event(EventType.ECPD, i, tasks(i).getECT, tasks(i).getMinResource)		
			}

			if (tasks(i).getLST >= tasks(i).getECT) {
				
				// Generates events
				hEvents enqueue new Event(EventType.CCP, i, tasks(i).getLST, 0)
			}
			
			if (tasks(i).getEST != tasks(i).getLST) {
				
				// Generates events
				hEvents enqueue new Event(EventType.PR, i, tasks(i).getEST, 0)
			}			
		}
	}
	
	def sweepAlgorithm(tasks : Array[CumulativeActivity]) : CPOutcome = {
		
		// INITIALIZATION
		initDataStructures(tasks)
		
		while (!hEvents.isEmpty) {
			
			// HANDLING THE SWEEP-LINE MOVE
			if (delta != deltaBis) {
				
				while (!newActiveTasks.isEmpty) {
				
					val t = newActiveTasks.dequeue
					
					if (tasks(t).getMinResource > gap) {
						
						hConflict.enqueue((tasks(t).getMinResource, t))
						hConflictSet.add(t)

					} else if (tasks(t).getMinDuration > deltaBis - delta) {

						hCheck.enqueue((tasks(t).getMinResource, t))
						hCheckSet.add(t)
						mins(t) = delta
						
					} else {
						evup(t) = true
					}
				}
				
				if (!filterMin(tasks, delta, deltaBis)) return CPOutcome.Failure
					
				delta = deltaBis
			}
			
			// HANDLING CURRENT EVENT
			delta = synchronize(tasks, delta)
			
			val event = hEvents.dequeue
			
			if (event.isSCP || event.isECPD) {
				
				gap = gap + event.dec
				
			} else if (event.isPR) {
				
				newActiveTasks.enqueue(event.task)
			}
			
			// GETTING NEXT EVENT
			if (hEvents.isEmpty && !filterMin(tasks, delta, Int.MaxValue)) return CPOutcome.Failure
				
			deltaBis = synchronize(tasks, delta)
		}
		
		return CPOutcome.Suspend
	}
	
	def filterMin(tasks : Array[CumulativeActivity], delta : Int, deltaBis : Int) : Boolean = {
		
		if (gap < 0) 
			return false
		
		// UPDATING TOP TASKS OF hCheck
					
		while (!hCheck.isEmpty && (hEvents.isEmpty || hCheck.head._1 > gap)) {
			
			val check = hCheck.dequeue
			val t = check._2
			val h = check._1
			hCheckSet.remove(t)
			
			if (delta >= tasks(t).getLST || delta - mins(t) >= tasks(t).getMinDuration || hEvents.isEmpty) {
				
				if (adjustStart(tasks(t), mins(t)) == CPOutcome.Failure) return false
				
				if (!evup(t)) {
					//Update events of the compulsory part of t
					evup(t) = true
				}
			} else {
				hConflict.enqueue(check)
				hConflictSet.add(t)
			}
		}	
		
		// UPDATING TOP TASKS OF hConflict
			
		while(!hConflict.isEmpty && hConflict.head._1 <= gap) {
			
			val conflict = hConflict.dequeue
			val t = conflict._2
			val h = conflict._1
			hConflictSet.remove(t)
			
			if (delta >= tasks(t).getLST) {
				
				if (adjustStart(tasks(t), tasks(t).getLST) == CPOutcome.Failure) return false
				
				if (!evup(t)) {
					//Update events of the compulsory part of t
					evup(t) = true
				}
			} else {
				
				if (deltaBis - delta >= tasks(t).getMinDuration) {
					
					if (adjustStart(tasks(t), delta) == CPOutcome.Failure) return false
				
					adjustStart(tasks(t), delta)
					
					if (!evup(t)) {
						
						//Update events of the compulsory part of t
						evup(t) = true
					}
				} else {
					hCheck.enqueue(conflict)
					hCheckSet.add(t)
					mins(t) = delta
				}
			}
		}
		
			
		true
	}
	
	def synchronize(tasks : Array[CumulativeActivity], delta : Int) : Int = {
		
		var sync = false
		var event : Event = null
		
		// UPDATING TOP EVENTS
		do {
			
			if (hEvents.isEmpty) return -1
				
			sync = true 
			
			event = hEvents.head
			
			// PROCESSING DYNAMIC EVENT
			if (event.isECPD && !evup(event.task)) {
				
				if (hCheckSet.contains(event.task)) {
					
					hEvents.dequeue
					hEvents.enqueue(new Event(event.eType, event.task, mins(event.task) + tasks(event.task).getMinDuration, event.dec))
					
				} else if (hConflictSet.contains(event.task)) {	
					
					hEvents.dequeue
					hEvents.enqueue(new Event(event.eType, event.task, tasks(event.task).getLST + tasks(event.task).getMinDuration, event.dec))	
				}
				
				evup(event.task) = true
				sync = false
				
			// PROCESSING CONDITIONAL EVENT	
			} else if (event.isCCP && !evup(event.task) && event.date == delta) {
				
				if (hCheckSet.contains(event.task) && mins(event.task) + tasks(event.task).getMinDuration > delta) {
					
					hEvents.enqueue(new Event(EventType.SCP, event.task, delta, -tasks(event.task).getMinResource))
					hEvents.enqueue(new Event(EventType.ECPD, event.task, mins(event.task) + tasks(event.task).getMinDuration, tasks(event.task).getMinResource))
					
				} else if (hConflictSet.contains(event.task)) {	
					
					hEvents.enqueue(new Event(EventType.SCP, event.task, delta, -tasks(event.task).getMinResource))
					hEvents.enqueue(new Event(EventType.ECPD, event.task, tasks(event.task).getLCT, tasks(event.task).getMinResource))	
				}
			
				evup(event.task) = true
				sync = false
			}
			
		} while (!sync)
			
		return event.date
	}

	/** The Event
	 */
	object EventType extends Enumeration {
		
		type EventType = Value
		
		val SCP  = Value("SCP")
		val ECPD = Value("ECPD")
		val CCP  = Value("CCP")
		val PR   = Value("PR")
	}
	
	import EventType._
	
	class Event(e : EventType, t : Int, d : Int, inc : Int) extends Enumeration {

		def isSCP  = { e == EventType.SCP }
		def isECPD = { e == EventType.ECPD }
		def isCCP  = { e == EventType.CCP }
		def isPR   = { e == EventType.PR }
		
		def date  = d
		def eType = e
		def dec   = inc
		def task  = t
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + inc +">" }
	}
	
	def adjustStart(t : CumulativeActivity, v : Int) = {
		
		if (!t.isInstanceOf[MirrorActivity])
			t.getStart.updateMin(v)
		else			
			t.getEnd.updateMax(-v)
	}
	
	class MirrorActivity(act : CumulativeActivity) extends CumulativeActivity(act.getStart, act.getDur, act.getEnd, act.getMachines, act.getResource) {
		
		override def getEST = -super.getLCT
		
		override def getLST = -super.getECT
		
		override def getECT = -super.getLST
		
		override def getLCT = -super.getEST
	}
}