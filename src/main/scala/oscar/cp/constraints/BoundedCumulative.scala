package oscar.cp.constraints

import scala.math.max
import scala.math.min
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

class BoundedCumulative (cp: CPSolver, allTasks : Array[CumulativeActivity], lb : Int, ub : Int, r : Int) extends Constraint(allTasks(0).machine.getStore(), "BoundedCumulative") {

	val tasks = allTasks.filter(a => a.machine.hasValue(r))
	
	val nTasks = tasks.size
	val Tasks  = 0 until nTasks
	
	// Event Point Series (min heap on date)
	val eventPointSeries = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = if (b.date > a.date) {1} else if (b.date == a.date) {0} else {-1} })

	// Sweep line parameters
	var delta         : Int = 0			// Position of the line
	var consSumHeight : Int = 0			// Height of the profile
	var capaSumHeight : Int = 0			// Height of the profile
	var nCurrentTasks : Int = 0			// Tasks overlaping the line
	val stackPrune : Set[Int] = Set()	// Tasks to prune

	// Capacities added to consSumHeight during a sweep
	val consContrib = new Array[Int](nTasks)
	val capaContrib = new Array[Int](nTasks)
	
	// True if the internal fix point is reached
	var fixPoint : Boolean = false

	override def setup(l: CPPropagStrength) : CPOutcome = {

		//setIdempotent
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- Tasks) {
        		
        		if (!tasks(i).start.isBound) tasks(i).start.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).dur.isBound) tasks(i).dur.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).dur.isBound) tasks(i).end.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).resource.isBound) tasks(i).resource.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).resource.isBound) tasks(i).machine.callPropagateWhenDomainChanges(this)
        	}
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
		
		var fixPoint = false
		
		//while (!fixPoint) {
			
			fixPoint = true
			
			// fixPoint is modified during the sweep
			if (sweepAlgorithm == CPOutcome.Failure) return CPOutcome.Failure
		//}
        
		return CPOutcome.Suspend
	}
	
	def nextEvent = if (eventPointSeries.size > 0) eventPointSeries.dequeue else null
	
	def generateEventPointSeries : Boolean = {
		
		// True if a profile event has been generated
		var profileEvent = false
		
		// Reset eventPointSeries
		eventPointSeries.clear
		
		for (i <- Tasks) {
			
			if (tasks(i).lst < tasks(i).ect && tasks(i).machine.isBoundTo(r)) {
				
				// Check
				if (tasks(i).maxResource < max(0, lb)) {
					
					// Generates events
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).lst, 1, 1)
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).ect, -1, -1)
				}
				
				// Profile (Bad : on compulsory part)
				val capaInc = max(0, tasks(i).minResource)
				val consInc = min(0, tasks(i).maxResource)
				
				if (capaInc != 0 || consInc != 0) {
					
					// Generates events
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).lst, consInc, capaInc)  
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).ect, -consInc, -capaInc) 
					
					profileEvent = true
				}			
			}
			
			if (tasks(i).machine.hasValue(r)) {
				
				// Profile (Good : on entire domain)
				val capaInc = min(0, tasks(i).minResource)
				val consInc = max(0, tasks(i).maxResource)
				
				if (capaInc != 0 || consInc != 0) {
					
					// Generates events		
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).est, consInc, capaInc)  
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).lct, -consInc, -capaInc) 
					
					profileEvent = true
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).start.isBound && tasks(i).end.isBound && tasks(i).machine.isBoundTo(r) && tasks(i).resource.isBound)) {
					
					// Generates event
					eventPointSeries enqueue new Event(EventType.Pruning, i, tasks(i).est, 0, 0)
				}
			}			
		}
		
		profileEvent
	}
	
	def resetSweepLine = {
			
		delta         = 0
		consSumHeight = 0
		capaSumHeight = 0
		nCurrentTasks = 0
		stackPrune.clear
			
		for (i <- Tasks) {
			consContrib(i) = 0
			capaContrib(i) = 0
		}
	}

	def sweepAlgorithm : CPOutcome = {
		
		// Reset the parameters of the sweep line
		resetSweepLine
		
		// Generate events (no need to sort them as we use a priorityQueue)
		if (!generateEventPointSeries) 
			return CPOutcome.Suspend
		
		var event = nextEvent
		var delta = event.date
		
		while (event != null) {
		
			if (!event.isPruningEvent) {
				
				// If we have considered all the events of the previous date
				if (delta != event.date) {
					
					// Consistency check
					if ((nCurrentTasks > 0 && consSumHeight < lb) ||
						 capaSumHeight > ub) 
						return CPOutcome.Failure
					
					// Pruning (this will empty the stackPrune list)
					if (prune(delta, event.date - 1) == CPOutcome.Failure) 
						return CPOutcome.Failure
						
					// New date to consider
					delta = event.date	
				}
				
				if (event.isCheckEvent) {
					
					nCurrentTasks += event.cons
					
				} else if (event.isProfileEvent) {
					
					consSumHeight += event.cons
					consContrib(event.task) += event.cons
					
					capaSumHeight += event.capa
					capaContrib(event.task) += event.capa
				}
			}
			else {
				stackPrune add event.task
			}
			
			event = nextEvent
		}
		
		// Consistency check
		if ((nCurrentTasks > 0 && consSumHeight < lb) ||
			 capaSumHeight > ub) 
			return CPOutcome.Failure
			
		// Pruning
		if (prune(delta, Int.MaxValue) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		return CPOutcome.Suspend
	}
	
	def prune(low : Int, up : Int) : CPOutcome = {
		
		val it = stackPrune.iterator
		
		while (!it.isEmpty) {
			
			val t = it.next
			
			if (pruneMandatory(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			if (pruneForbiden(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			if (pruneConsumption(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			if (tasks(t).lct <= up + 1) {
				stackPrune.remove(t)
			}
		}	

		return CPOutcome.Suspend
	}
	
	def pruneMandatory(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		// Consistency check
		if ((nCurrentTasks == 0 || (consSumHeight - consContrib(t)) >= lb) && (capaSumHeight - capaContrib(t) <= ub))
			return CPOutcome.Suspend
		
		// Fix the activity to the machine r and check consistency
		if (fixVar(tasks(t).machine, r) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the EST of the activity and check consistency
		if (adjustMin(tasks(t).start, up - tasks(t).maxDuration + 1) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the LST of the activity and check consistency
		if (adjustMax(tasks(t).start, low) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the LCT of the activity and check consistency
		if (adjustMax(tasks(t).end, low + tasks(t).maxDuration) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the ECT of the activity and check consistency
		if (adjustMin(tasks(t).end, up + 1) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the minimal duration of the activity and check consistency
		if (adjustMin(tasks(t).dur, min(up - tasks(t).lst+1, tasks(t).ect-low)) == CPOutcome.Failure) 
			return CPOutcome.Failure
			
		return CPOutcome.Suspend
	}
	
	def pruneForbiden(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if ((consSumHeight - consContrib(t) + tasks(t).maxResource < lb) || 
		    (capaSumHeight - capaContrib(t) + tasks(t).minResource > ub)) {
			
			if (tasks(t).ect > low && tasks(t).lst <= up && tasks(t).minDuration > 0) {
					
				if (removeValue(tasks(t).machine, r) == CPOutcome.Failure) 
					return CPOutcome.Failure
				
			} else if (tasks(t).machine.isBoundTo(r)) {
				
				if (tasks(t).minDuration > 0) {
					
					//INTERVAL PRUNING
					for (i <- low - tasks(t).minDuration+1 to up) {
						
						if (removeValue(tasks(t).start, i) == CPOutcome.Failure) 
							return CPOutcome.Failure
					}
					
					for (i <- low + 1 to up + tasks(t).minDuration) {
						
						if (removeValue(tasks(t).end, i) == CPOutcome.Failure) 
							return CPOutcome.Failure
					}
				}
				
				val maxD = max(max(low - tasks(t).est, tasks(t).lct -up - 1), 0)
				
				if (adjustMax(tasks(t).dur, maxD) == CPOutcome.Failure) 
					return CPOutcome.Failure
			}
		}
			
		return CPOutcome.Suspend
	}
	
	def pruneConsumption(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (tasks(t).machine.isBoundTo(r) && tasks(t).ect > low && tasks(t).lst <= up && tasks(t).minDuration > 0) {
			
			if (adjustMin(tasks(t).resource, lb - (consSumHeight - consContrib(t))) == CPOutcome.Failure) 
				
			if (adjustMax(tasks(t).resource, ub - (capaSumHeight - capaContrib(t))) == CPOutcome.Failure) 
				return CPOutcome.Failure
		}
			
		return CPOutcome.Suspend
	}
	
	/** The Event
	 */
	object EventType extends Enumeration {
		
		type EventType = Value
		
		val Check   = Value("Check event")
		val Profile = Value("Profile event")
		val Pruning = Value("Pruning event")
	}
	
	import EventType._
	
	class Event(e : EventType, t : Int, d : Int, consomation : Int, capacity : Int) extends Enumeration {

		def isCheckEvent   = { e == EventType.Check }
		def isProfileEvent = { e == EventType.Profile }
		def isPruningEvent = { e == EventType.Pruning }
		
		def date  = d
		def eType = e
		def cons  = consomation
		def capa  = capacity
		def task  = t
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + cons +">" }
	}
	
	def adjustMin(x : CPVarInt, v : Int) : CPOutcome = {
		
		val min = x.getMin
		val oc  = x.updateMin(v)
		
		fixPoint &= min != x.getMin
		
		return oc
	}
	
	def adjustMax(x : CPVarInt, v : Int) : CPOutcome = {
		
		val max = x.getMax
		val oc  = x.updateMax(v)
		
		fixPoint &= max != x.getMax
		
		return oc
	}
	
	def fixVar(x: CPVarInt, v : Int) : CPOutcome = {
		
		val size = x.getSize
		val oc   = x.assign(v)
		
		fixPoint &= size > 1
		
		return oc
	}
	
	def removeValue(x: CPVarInt, v : Int) : CPOutcome = {
		
		val size = x.getSize
		val oc   = x.removeValue(v)
		
		fixPoint &= x.getSize == size-1
		
		return oc
	}
}