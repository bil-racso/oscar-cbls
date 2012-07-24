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

class NewMaxCumulative(cp: CPSolver, allTasks : Array[CumulativeActivity], limit : Int, r : Int)  extends Constraint(allTasks(0).machine.getStore(), "NewMaxCumulative") {
	
	val lToRTasks : Array[CumulativeActivity] = allTasks.filter(_.machine.hasValue(r))
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
	
	val eventList = Array.tabulate(nTasks){e => new EventList(e)}

	override def setup(l: CPPropagStrength) : CPOutcome = {
		
		setIdempotent
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- Tasks) {
    		
        		if (!lToRTasks(i).start.isBound) lToRTasks(i).start.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).dur.isBound) lToRTasks(i).dur.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).end.isBound) lToRTasks(i).end.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).resource.isBound) lToRTasks(i).resource.callPropagateWhenBoundsChange(this)
	        	if (!lToRTasks(i).machine.isBound) lToRTasks(i).machine.callPropagateWhenDomainChanges(this)
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
		
		for (i <- Tasks; if (tasks(i).machine.isBoundTo(r))) {
			
			if (tasks(i).lst < tasks(i).ect) {
				
				// Generates events
				hEvents enqueue eventList(i).getSCP(tasks)
				hEvents enqueue eventList(i).getECPD(tasks)
				//hEvents enqueue new Event(EventType.SCP, i, tasks(i).lst, -tasks(i).minResource)
				//hEvents enqueue new Event(EventType.ECPD, i, tasks(i).ect, tasks(i).minResource)		
			}

			if (tasks(i).lst >= tasks(i).ect) {
				
				// Generates events
				hEvents enqueue eventList(i).getCCP(tasks)
				//hEvents enqueue new Event(EventType.CCP, i, tasks(i).lst, 0)
			}
			
			if (tasks(i).est != tasks(i).lst) {
				
				// Generates events
				hEvents enqueue eventList(i).getPR(tasks)
				//hEvents enqueue new Event(EventType.PR, i, tasks(i).est, 0)
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
					
					if (tasks(t).minResource > gap) {
						
						hConflict.enqueue((tasks(t).minResource, t))
						hConflictSet.add(t)

					} else if (tasks(t).minDuration > deltaBis - delta) {

						hCheck.enqueue((tasks(t).minResource, t))
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
			
			if (delta >= tasks(t).lst || delta - mins(t) >= tasks(t).minDuration || hEvents.isEmpty) {
				
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
			
			if (delta >= tasks(t).lst) {
				
				if (adjustStart(tasks(t), tasks(t).lst) == CPOutcome.Failure) return false
				
				if (!evup(t)) {
					//Update events of the compulsory part of t
					evup(t) = true
				}
			} else {
				
				if (deltaBis - delta >= tasks(t).minDuration) {
					
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
					hEvents.enqueue(new Event(event.eType, event.task, mins(event.task) + tasks(event.task).minDuration, event.dec))
					
				} else if (hConflictSet.contains(event.task)) {	
					
					hEvents.dequeue
					hEvents.enqueue(new Event(event.eType, event.task, tasks(event.task).lst + tasks(event.task).minDuration, event.dec))	
				}
				
				evup(event.task) = true
				sync = false
				
			// PROCESSING CONDITIONAL EVENT	
			} else if (event.isCCP && !evup(event.task) && event.date == delta) {
				
				if (hCheckSet.contains(event.task) && mins(event.task) + tasks(event.task).minDuration > delta) {
					
					hEvents.enqueue(new Event(EventType.SCP, event.task, delta, -tasks(event.task).minResource))
					hEvents.enqueue(new Event(EventType.ECPD, event.task, mins(event.task) + tasks(event.task).minDuration, tasks(event.task).minResource))
					
				} else if (hConflictSet.contains(event.task)) {	
					
					hEvents.enqueue(new Event(EventType.SCP, event.task, delta, -tasks(event.task).minResource))
					hEvents.enqueue(new Event(EventType.ECPD, event.task, tasks(event.task).lct, tasks(event.task).minResource))	
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
	
	class Event(e : EventType, t : Int, private var d : Int, private var inc : Int) extends Enumeration {

		def isSCP  = { e == EventType.SCP }
		def isECPD = { e == EventType.ECPD }
		def isCCP  = { e == EventType.CCP }
		def isPR   = { e == EventType.PR }
		
		def date  = d
		def eType = e
		def dec   = inc
		def task  = t
		
		def date_= (x : Int) {d = x}
		def dec_= (x : Int) {inc = x}
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + inc +">" }
	}
	
	class EventList(t : Int) {
		
		val SCP  : Event = new Event(EventType.SCP , t, 0, 0)
		val ECPD : Event = new Event(EventType.ECPD, t, 0, 0)
		val CCP  : Event = new Event(EventType.CCP , t, 0, 0)
		val PR   : Event = new Event(EventType.PR  , t, 0, 0)
		
		def getSCP(tasks : Array[CumulativeActivity]) : Event = {
			SCP.date = tasks(t).lst
			SCP.dec  = -tasks(t).minResource
			return SCP
		}
		
		def getECPD(tasks : Array[CumulativeActivity]) : Event = {
			ECPD.date = tasks(t).ect
			ECPD.dec  = tasks(t).minResource
			return ECPD
		}
		
		def getCCP(tasks : Array[CumulativeActivity]) : Event = {
			CCP.date = tasks(t).lst
			return CCP
		}
		
		def getPR(tasks : Array[CumulativeActivity]) : Event = {
			PR.date = tasks(t).est
			return PR
		}
	}
	
	def adjustStart(t : CumulativeActivity, v : Int) = {
		
		if (!t.isInstanceOf[MirrorActivity])
			t.start.updateMin(v)
		else			
			t.end.updateMax(-v)
	}
	
	class MirrorActivity(act : CumulativeActivity) extends CumulativeActivity(act.start, act.dur, act.end, act.machine, act.resource) {
		
		override def est = -super.lct
		
		override def lst = -super.ect
		
		override def ect = -super.lst
		
		override def lct = -super.est
	}
}