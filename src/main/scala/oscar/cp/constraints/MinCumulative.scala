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

/**
 * 
 */
class MinCumulative (cp: CPSolver, tasks : Array[CumulativeActivity], limit : Int, r : Int, domain : Boolean) extends Constraint(tasks(0).getMachines.getStore(), "MaxCumulative") {

	val nTasks = tasks.size
	val Tasks  = 0 until nTasks
	
	// Event Point Series (min heap on date)
	val eventPointSeries = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = if (b.date > a.date) {1} else if (b.date == a.date) {0} else {-1} })

	// Sweep line parameters
	var delta         : Int = 0			// Position of the line
	var sumHeight     : Int = 0			// Height of the profile
	var nCurrentTasks : Int = 0			// Tasks overlaping the line
	val stackPrune : Set[Int] = Set()	// Tasks to prune

	// Capacities added to sumHeight during a sweep
	val contribution = new Array[Int](nTasks)
	
	// True if the internal fix point is reached
	var fixPoint : Boolean = false

	override def setup(l: CPPropagStrength) : CPOutcome = {

		setIdempotent
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- Tasks) {

        			
        		if (domain) {
        			
        			if (!tasks(i).getStart.isBound) tasks(i).getStart.callPropagateWhenDomainChanges(this)
        			if (!tasks(i).getEnd.isBound) tasks(i).getEnd.callPropagateWhenDomainChanges(this)
        			if (!tasks(i).getDur.isBound) tasks(i).getDur.callPropagateWhenDomainChanges(this)
        			if (!tasks(i).getResource.isBound) tasks(i).getResource.callPropagateWhenDomainChanges(this)
        			if (!tasks(i).getMachines.isBound) tasks(i).getMachines.callPropagateWhenDomainChanges(this)
        		
        		} else {
        		
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
		
		var fixPoint = false
		
		while (!fixPoint) {
			
			fixPoint = true
			
			// fixPoint is modified during the sweep
			if (sweepAlgorithm(r) == CPOutcome.Failure) return CPOutcome.Failure
		}
        
		return CPOutcome.Suspend
	}
	
	def nextEvent = if (eventPointSeries.size > 0) eventPointSeries.dequeue else null
	
	def generateEventPointSeries(r : Int) : Boolean = {
		
		// True if a profile event has been generated
		var profileEvent = false
		
		// Reset eventPointSeries
		eventPointSeries.clear
		
		for (i <- Tasks) {
			
			if (tasks(i).hasCompulsoryPart && tasks(i).getMachines.isBoundTo(r)) {
				
				// Check
				if (tasks(i).getMaxResource < limit) {
					
					// Generates events
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).getLST, 1)
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).getECT, -1)
				}
				
				// Profile (Bad : on compulsory part)
				if (tasks(i).getMaxResource < 0) {
					
					// Generates events
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getLST, tasks(i).getMaxResource)  
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getECT, -tasks(i).getMaxResource) 
					
					profileEvent = true
				}			
			}
			
			if (tasks(i).getMachines.hasValue(r)) {
				
				// Profile (Good : on entire domain)
				if (tasks(i).getMaxResource > 0) {
					
					// Generates events		
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getEST, tasks(i).getMaxResource)  
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getLCT, -tasks(i).getMaxResource) 
					
					profileEvent = true
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).getStart.isBound && tasks(i).getEnd.isBound && tasks(i).getMachines.isBoundTo(r) && tasks(i).getResource.isBound)) {
					
					// Generates event
					eventPointSeries enqueue new Event(EventType.Pruning, i, tasks(i).getEST, 0)
				}
			}			
		}
		
		profileEvent
	}
	
	def resetSweepLine = {
			
		delta         = 0
		sumHeight     = 0
		nCurrentTasks = 0
		stackPrune.clear
			
		for (i <- Tasks) {
			contribution(i) = 0
		}
	}

	def sweepAlgorithm(r : Int) : CPOutcome = {
		
		// Reset the parameters of the sweep line
		resetSweepLine
		
		// Generate events (no need to sort them as we use a priorityQueue)
		if (!generateEventPointSeries(r)) 
			return CPOutcome.Suspend
		
		var event = nextEvent
		var delta = event.date
		
		while (event != null) {
		
			if (!event.isPruningEvent) {
				
				// If we have considered all the events of the previous date
				if (delta != event.date) {
					
					// Consistency check
					if (nCurrentTasks > 0 && sumHeight < limit) 
						return CPOutcome.Failure
					
					// Pruning (this will empty the stackPrune list)
					if (prune(r, delta, event.date - 1) == CPOutcome.Failure) 
						return CPOutcome.Failure
						
					// New date to consider
					delta = event.date	
				}
				
				if (event.isCheckEvent) {
					
					nCurrentTasks += event.increment
					
				} else if (event.isProfileEvent) {
					
					sumHeight += event.increment
					contribution(event.task) += event.increment
				}
			}
			else {
				stackPrune add event.task
			}
			
			event = nextEvent
		}
		
		// Consistency check
		if (nCurrentTasks > 0 && sumHeight < limit) 
			return CPOutcome.Failure
			
		// Pruning
		if (prune(r, delta, delta) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		return CPOutcome.Suspend
	}
	
	def prune(r : Int, low : Int, up : Int) : CPOutcome = {
		
		val it = stackPrune.iterator
		
		while (!it.isEmpty) {
			
			val t = it.next
			
			if (pruneMandatory(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			if (pruneForbiden(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			if (pruneConsumption(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			if (tasks(t).getLCT <= up + 1) {
				stackPrune.remove(t)
			}
		}	

		return CPOutcome.Suspend
	}
	
	def pruneMandatory(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		// Consistency check
		if (nCurrentTasks == 0 || (sumHeight - contribution(t)) >= limit) {
			return CPOutcome.Suspend
		}
		
		// Fix the activity to the machine r and check consistency
		if (fixVar(tasks(t).getMachines, r) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the EST of the activity and check consistency
		if (adjustMin(tasks(t).getStart, up - tasks(t).getMaxDuration + 1) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the LST of the activity and check consistency
		if (adjustMax(tasks(t).getStart, low) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the LCT of the activity and check consistency
		if (adjustMax(tasks(t).getEnd, low + tasks(t).getMaxDuration) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the ECT of the activity and check consistency
		if (adjustMin(tasks(t).getEnd, up + 1) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the minimal duration of the activity and check consistency
		if (adjustMin(tasks(t).getDur, min(up - tasks(t).getLST+1, tasks(t).getECT-low)) == CPOutcome.Failure) 
			return CPOutcome.Failure
			
		return CPOutcome.Suspend
	}
	
	def pruneForbiden(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (sumHeight - contribution(t) + tasks(t).getMaxResource < limit) {
			
			if (tasks(t).getECT > low && tasks(t).getLST <= up && tasks(t).getMinDuration > 0) {
					
				if (removeValue(tasks(t).getMachines, r) == CPOutcome.Failure) 
					return CPOutcome.Failure
				
			} else if (tasks(t).getMachines.isBoundTo(r)) {
				
				if (tasks(t).getMinDuration > 0) {
					
					//INTERVAL PRUNING
					for (i <- low - tasks(t).getMinDuration+1 to up) {
						
						if (removeValue(tasks(t).getStart, i) == CPOutcome.Failure) 
							return CPOutcome.Failure
					}
					
					for (i <- low + 1 to up + tasks(t).getMinDuration) {
						
						if (removeValue(tasks(t).getEnd, i) == CPOutcome.Failure) 
							return CPOutcome.Failure
					}
				}
				
				val maxD = max(max(low - tasks(t).getEST, tasks(t).getLCT -up - 1), 0)
				
				if (adjustMax(tasks(t).getDur, maxD) == CPOutcome.Failure) 
					return CPOutcome.Failure
			}
		}
			
		return CPOutcome.Suspend
	}
	
	def pruneConsumption(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (tasks(t).getMachines.isBoundTo(r) && tasks(t).getECT > low && tasks(t).getLST <= up && tasks(t).getMinDuration > 0) {
			
			if (adjustMin(tasks(t).getResource, limit - (sumHeight - contribution(t))) == CPOutcome.Failure) 
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
	
	class Event(e : EventType, t : Int, d : Int, inc : Int) extends Enumeration {

		def isCheckEvent   = { e == EventType.Check }
		def isProfileEvent = { e == EventType.Profile }
		def isPruningEvent = { e == EventType.Pruning }
		
		def date      = d
		def eType     = e
		def increment = inc
		def task      = t
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + inc +">" }
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