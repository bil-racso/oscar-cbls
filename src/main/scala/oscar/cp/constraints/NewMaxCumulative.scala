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
import org.hamcrest.core.IsEqual

class NewMaxCumulative(cp: CPSolver, tasks : Array[CumulativeActivity], limit : Int, r : Int)  extends Constraint(tasks(0).getMachines.getStore(), "NewMaxCumulative") {
	
	val hEvents    = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = if (b.date > a.date) {1} else if (b.date == a.date) {0} else {-1} })
	
	val hCheck    = new PriorityQueue[Tuple2[Int, Int]]()(new Ordering[Tuple2[Int, Int]] { def compare(a : Tuple2[Int, Int], b : Tuple2[Int, Int]) = if (b._1 > a._1) {1} else if (b._1 == a._1) {0} else {-1} })
	val hConflict = new PriorityQueue[Tuple2[Int, Int]]()(new Ordering[Tuple2[Int, Int]] { def compare(a : Tuple2[Int, Int], b : Tuple2[Int, Int]) = if (b._1 > a._1) {1} else if (b._1 == a._1) {0} else {-1} })
	
	val hCheckSet = Set[Int]()
	
	val newActiveTasks : Queue[Int] = Queue()
	
	val evup = new Array[Boolean](tasks.size)
	val mins = new Array[Int](tasks.size)
	
	var delta    = 0
	var deltaBis = 0	
	var gap	     = 0

	override def setup(l: CPPropagStrength) : CPOutcome = {
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- 0 until tasks.size) {
        		if (true){
      			
	        		if (!tasks(i).getStart.isBound) tasks(i).getStart.callPropagateWhenBoundsChange(this)
		        	if (!tasks(i).getDur.isBound) tasks(i).getDur.callPropagateWhenBoundsChange(this)
		        	if (!tasks(i).getDur.isBound) tasks(i).getEnd.callPropagateWhenBoundsChange(this)
		        	if (!tasks(i).getResource.isBound) tasks(i).getResource.callPropagateWhenBoundsChange(this)
		        	if (!tasks(i).getMachines.isBound) tasks(i).getMachines.callPropagateWhenDomainChanges(this)
        		}
        	}
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
			
		// fixPoint is modified during the sweep
		if (sweepAlgorithm == CPOutcome.Failure) return CPOutcome.Failure
        
		return CPOutcome.Suspend
	}

	def generateEvents {
		
		// Reset hEvents
		hEvents.clear
		
		for (i <- 0 until tasks.size; if (tasks(i).getMachines.isBoundTo(r))) {
			
			if (tasks(i).getLST < tasks(i).getECT) {
				
				// Generates events
				hEvents enqueue new Event(EventType.SCP, i, tasks(i).getLST, -tasks(i).getMaxResource)
				hEvents enqueue new Event(EventType.ECPD, i, tasks(i).getECT, tasks(i).getMaxResource)		
			}

			if (tasks(i).getLST >= tasks(i).getECT) {
				
				// Generates events
				hEvents enqueue new Event(EventType.CCP, i, tasks(i).getLST, 0)
			}
			
			if (tasks(i).getEST >= tasks(i).getLST) {
				
				// Generates events
				hEvents enqueue new Event(EventType.PR, i, tasks(i).getEST, 0)
			}			
		}
	}
	
	def initDataStructures {
		
	}
	
	def sweepAlgorithm : CPOutcome = {
		
		//Generate events
		generateEvents
		
		// Init data structure
		initDataStructures
		
		while (!hEvents.isEmpty) {
			
			//HANDLING THE SWEEP-LINE MOVE
			if (delta != deltaBis) {
				
				while (!newActiveTasks.isEmpty) {
					
					//Extract first task of newActiveTasks
					val t = newActiveTasks.dequeue
					
					if (tasks(t).getMaxResource > gap) {
						
						hConflict.enqueue((tasks(t).getMaxResource, t))

					} else if (tasks(t).getMaxDuration > deltaBis - delta) {

						hCheck.enqueue((tasks(t).getMaxResource, t))
						mins(t) = delta
						
					} else {
						evup(t) = true
					}
				}
				
				if (!filterMin(delta, deltaBis, gap)) 
					return CPOutcome.Failure
					
				delta = deltaBis
			}
			
			//HANDLING CURRENT EVENT
			delta = synchronize(delta)
			//extract event from eventPointSeries
			val event : Event = null
			
			if (event.isSCP || event.isECPD) {
				
				gap = gap + event.dec
				
			} else if (event.isPR) {
				
				newActiveTasks.enqueue(event.task)
			}
			
			//GETTING NEXT EVENT
			if (hEvents.isEmpty && !filterMin(delta, delta, gap))
				return CPOutcome.Failure
				
			deltaBis = synchronize(delta)
		}
		
		return CPOutcome.Suspend
	}
	
	def filterMin(delta : Int, deltaBis : Int, gap : Int) : Boolean = {
		
		if (gap < 0) 
			return false
		
		//UPDATING TOP TASKS OF hCheck
		if (!hCheck.isEmpty) {
			
			val check = hCheck.dequeue
			val t = check._2
			val h = check._1
			hCheckSet.remove(t)
					
			while (hEvents.isEmpty || h > gap) {
				
				if (delta >= tasks(t).getLST || delta - mins(t) >= tasks(t).getMaxDuration || hEvents.isEmpty) {
					
					adjustStart(t, mins(t), true)
					
					if (!evup(t)) {
						//Update events of the compulsory part of t
						evup(t) = true
					}
				} else {
					hConflict.enqueue(check)
				}
			}	
		}	
		
		//UPDATING TOP TASKS OF hConflict
		if (!hConflict.isEmpty) {
			
			val conflict = hConflict.dequeue
			val t = conflict._2
			val h = conflict._1
			
			while(h <= gap) {
				
				if (delta >= tasks(t).getLST) {
					
					adjustStart(t, tasks(t).getLST, true)
					
					if (!evup(t)) {
						//Update events of the compulsory part of t
						evup(t) = true
					}
				} else {
					
					if (deltaBis - delta >= tasks(t).getMaxDuration) {
						
						adjustStart(t, delta, true)
						
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
		}
			
		true
	}
	
	def synchronize(delta : Int) : Int = {
		
		var sync = false
		var event : Event = null
		
		//UPDATING TOP EVENTS
		do {
			
			if (hEvents.isEmpty)
				return -1
				
			sync = true 
			
			event = hEvents.dequeue
			
			//PROCESSING DYNAMIC EVENT
			if (event.isECPD && !evup(event.task)) {
				
				if (hCheckSet.contains(event.task)) {
					
					hEvents.enqueue(new Event(event.eType, event.task, mins(event.task) + tasks(event.task).getMaxDuration, event.dec))
					
				} else {	

					hEvents.enqueue(new Event(event.eType, event.task, tasks(event.task).getLST + tasks(event.task).getMaxDuration, event.dec))
					
				}
				
				evup(event.task) = true
				sync = false
				
			//PROCESSING CONDITIONAL EVENT	
			} else if (event.isCCP && !evup(event.task) && event.date == delta) {
				
				if (hCheckSet.contains(event.task) && mins(event.task) + tasks(event.task).getMaxDuration > delta) {
					
					hEvents.enqueue(new Event(EventType.SCP, event.task, delta, tasks(event.task).getMaxResource))
					hEvents.enqueue(new Event(EventType.ECPD, event.task, mins(event.task), -tasks(event.task).getMaxResource))
					
				} else {
					
					hEvents.enqueue(new Event(EventType.SCP, event.task, delta, tasks(event.task).getMaxResource))
					hEvents.enqueue(new Event(EventType.ECPD, event.task, mins(event.task), -tasks(event.task).getMaxResource))	
				}
			
				evup(event.task) = true
				sync = false
			}
			
		} while (sync)
			
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
	
	def adjustStart(t : Int, v : Int, lToR : Boolean) = {
		
		if (lToR)
			tasks(t).getStart.updateMin(v)
		else			
			tasks(t).getEnd.updateMax(v)
	}
}