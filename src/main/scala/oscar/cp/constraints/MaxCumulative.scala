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
class MaxCumulative (cp: CPSolver, tasks : Array[CumulativeActivity], limit : Int, r : Int, minCumulative : Boolean) extends Constraint(tasks(0).getMachines.getStore(), "MaxCumulative") {
	
	//val nTasks    = tasks.size
	
	// Event Point Series
	val eventPointSeries = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = b.date - a.date })

	// Sweep line parameters
	var delta      : Int = 0
	var sumHeight  : Int = 0
	val stackPrune : Set[Int] = Set()
	
	var fixPoint : Boolean = false

	val contribution = new Array[Int](tasks.size)
		
	def resetSweepLine = {
			
		sumHeight  = 0
		stackPrune.clear
			
		for (i <- 0 until tasks.size) {
			contribution(i) = 0
		}
	}
	
	
	override def setup(l: CPPropagStrength) : CPOutcome = {

		setIdempotent
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- 0 until tasks.size) {

        			
        		if (minCumulative) {
        			
        		if (!tasks(i).getStart.isBound) tasks(i).getStart.callPropagateWhenDomainChanges(this)
        		if (!tasks(i).getEnd.isBound) tasks(i).getEnd.callPropagateWhenDomainChanges(this)
        		if (!tasks(i).getDur.isBound) tasks(i).getDur.callPropagateWhenDomainChanges(this)
        		if (!tasks(i).getResource.isBound) tasks(i).getResource.callPropagateWhenDomainChanges(this)
        		if (!tasks(i).getMachines.isBound) tasks(i).getMachines.callPropagateWhenDomainChanges(this)
        		
        		} else {
        		
        		if (!tasks(i).getStart.isBound) tasks(i).getStart.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getEnd.isBound) tasks(i).getEnd.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getDur.isBound) tasks(i).getDur.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getResource.isBound) tasks(i).getResource.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getMachines.isBound) tasks(i).getMachines.callPropagateWhenDomainChanges(this)
        		
        		}
        	}
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
		
		var change = true
		var res : Tuple2[Boolean, CPOutcome] = null
		
		/*for(i <- 0 until limits.size) {
			inQueue(i) = true
			checkQueue.enqueue(i)
		}
		
		while (!checkQueue.isEmpty) {
			
			val r = checkQueue.dequeue
			inQueue(r) = false
			
			res = sweepAlgorithm(r)
			if (res._2 == CPOutcome.Failure) return CPOutcome.Failure
		}*/
		
		while (change) {
				
			res = sweepAlgorithm(r)
			change = res._1
			if (res._2 == CPOutcome.Failure) return CPOutcome.Failure
		}
        
		return CPOutcome.Suspend
	}
	
	def nextEvent = if (eventPointSeries.size > 0) eventPointSeries.dequeue else null
	
	def generateEventPointSeries(r : Int) : Boolean = {
		
		// True if a profile event has been generated
		var profileEvent = false
		
		// Reset eventPointSeries
		eventPointSeries.clear
		
		// For each cumulative activity
		for (i <- 0 until tasks.size) {
			
			if (tasks(i).hasCompulsoryPart && tasks(i).getMachines.isBoundTo(r)) {
				/*
				// Check
				if (tasks(i).getMaxResource < limit) {
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).getLST, 1)
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).getECT, -1)
				}*/
				
				// Profile (Bad)
				if (tasks(i).getMinResource > 0) {
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getLST, tasks(i).getMinResource)  
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getECT, -tasks(i).getMinResource) 
					
					profileEvent = true
				}			
			}
			
			if (tasks(i).getMachines.hasValue(r)) {
				
				// Profile (Good)
				if (tasks(i).getMinResource < 0) {
					
					// Generate events		
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getEST, tasks(i).getMinResource)  
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getLCT, -tasks(i).getMinResource) 
					
					profileEvent = true
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).getStart.isBound && tasks(i).getEnd.isBound && tasks(i).getMachines.isBoundTo(r) && tasks(i).getResource.isBound)) {
					
					// Generate event
					eventPointSeries enqueue new Event(EventType.Pruning, i, tasks(i).getEST, 0)
				}
			}			
		}
		
		profileEvent
	}

	def sweepAlgorithm(r : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		// Reset the parameters of the sweep line
		resetSweepLine
		
		// Generate events (no need to sort them as we use a priorityQueue)
		if (!generateEventPointSeries(r)) return (change, CPOutcome.Suspend)
		
		var event = nextEvent
		var d = event.date
		
		while (event != null) {
		
			if (!event.isPruningEvent) {
				
				// If we have considered all the events of the previous date
				if (d != event.date) {
					// Consistency check
					if (sumHeight > limit) return (change, CPOutcome.Failure)
					// Pruning (this will empty the stackPrune list)
					res = prune(r, d, event.date - 1)
					change |= res._1
					if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
					// New date to consider
					d = event.date	
				}
				
				/*if (event.isCheckEvent) {
					
					nTasks += event.increment
					
				} else*/ if (event.isProfileEvent) {
					
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
		if (sumHeight > limit) return (change, CPOutcome.Failure) // TODO
		// Pruning
		res = prune(r, d, d)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		return (change, CPOutcome.Suspend)
	}
	
	def prune(r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		val it = stackPrune.iterator
		
		while (!it.isEmpty) {
			
			val t = it.next
			
			res = pruneMandatory(t, r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = pruneForbiden(t, r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = pruneConsumption(t, r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			if (tasks(t).getLCT <= up + 1) {
				stackPrune.remove(t)
			}
		}	

		return (change, CPOutcome.Suspend)
	}
	
	def pruneMandatory(t : Int, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		// Consistency check
		if ((sumHeight - contribution(t)) <= limit) { // TODO
			return (change, CPOutcome.Suspend)
		}
		
		// Fix the activity to the machine r and check consistency
		res = MaxCumulative.fixVar(tasks(t).getMachines, r)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the EST of the activity and check consistency
		res = MaxCumulative.adjustMin(tasks(t).getStart, up - tasks(t).getMaxDuration + 1)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the LST of the activity and check consistency
		res = MaxCumulative.adjustMax(tasks(t).getStart, low)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the LCT of the activity and check consistency
		res = MaxCumulative.adjustMax(tasks(t).getEnd, low + tasks(t).getMaxDuration)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the ECT of the activity and check consistency
		res = MaxCumulative.adjustMin(tasks(t).getEnd, up + 1)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the minimal duration of the activity and check consistency
		res = MaxCumulative.adjustMin(tasks(t).getDur, min(up - tasks(t).getLST+1, tasks(t).getECT-low))
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
		return (change, CPOutcome.Suspend)
	}
	
	def pruneForbiden(t : Int, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		if (sumHeight - contribution(t) + tasks(t).getMinResource > limit) {
			
			if (tasks(t).getECT > low && tasks(t).getLST <= up && tasks(t).getMinDuration > 0) {
					
				res = MaxCumulative.removeValue(tasks(t).getMachines, r)
				change |= res._1
				if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
				
			} else if (tasks(t).getMachines.isBoundTo(r)) {
				
				if (tasks(t).getMinDuration > 0) {
					
					//INTERVAL PRUNING
					for (i <- low - tasks(t).getMinDuration+1 to up) {
						
						res = MaxCumulative.removeValue(tasks(t).getStart, i)
						change |= res._1
						if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
					}
					
					for (i <- low + 1 to up + tasks(t).getMinDuration) {
						
						res = MaxCumulative.removeValue(tasks(t).getEnd, i)
						change |= res._1
						if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
					}
				}
				
				val maxD = max(max(low - tasks(t).getEST, tasks(t).getLCT -up - 1), 0)
				res = MaxCumulative.adjustMax(tasks(t).getDur, maxD)
				change |= res._1
				if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			}
		}
			
		return (change, CPOutcome.Suspend)
	}
	
	def pruneConsumption(t : Int, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var res : Tuple2[Boolean, CPOutcome] = (false, CPOutcome.Suspend)
		
		if (tasks(t).getMachines.isBoundTo(r) && tasks(t).getECT > low && tasks(t).getLST <= up && tasks(t).getMinDuration > 0) {
			
			res = MaxCumulative.adjustMax(tasks(t).getResource, limit - (sumHeight - contribution(t)))
		}
			
		return res
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
}

object MaxCumulative extends CPModel {
	
	def adjustMin(x : CPVarInt, v : Int) : Tuple2[Boolean, CPOutcome] = {
		
		val min = x.getMin
		val oc  = x.updateMin(v)
		
		return (min != x.getMin, oc)
	}
	
	def adjustMax(x : CPVarInt, v : Int) : Tuple2[Boolean, CPOutcome] = {
		
		val max = x.getMax
		val oc  = x.updateMax(v)
		
		return (max != x.getMax, oc)
	}
	
	def adjustHeight(t : CumulativeActivity, v : Int, b : Boolean) : Tuple2[Boolean, CPOutcome] = {

		if (b) {
			return adjustMin(t.getResource, v)
		} else {		
			return adjustMax(t.getResource, v)
		}
	}
	
	def fixVar(x: CPVarInt, v : Int) : Tuple2[Boolean, CPOutcome] = {
		
		val size = x.getSize
		val oc   = x.assign(v)
		
		return (size > 1, oc)
	}
	
	def removeValue(x: CPVarInt, v : Int) : Tuple2[Boolean, CPOutcome] = {
		
		val size = x.getSize
		val oc   = x.removeValue(v)
		
		return (x.getSize == size-1, oc)
	}
}