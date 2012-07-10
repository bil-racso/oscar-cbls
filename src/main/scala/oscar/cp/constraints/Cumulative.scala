package oscar.cp.constraints

import scala.math.max
import scala.math.min
import scala.collection.mutable.PriorityQueue

import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.modeling.CPModel

/**
 * 
 * 
 * 
 */
class Cumulative(cp: CPSolver, tasks : Array[CumulativeActivity], lowerBound : Int, upperBound : Int, r : Int) extends Constraint(tasks(0).getMachines.getStore(), "Cumulative") {
	
	// Event Point Series
	val eventPointSeries = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = b.date - a.date })
	
	// Sweep Line 
	var sumHeight  : Int = 0 
	var nTasks     : Int = 0
	var stackPrune : List[Int] = Nil
		
	def resetSweepLine = {
			
		sumHeight  = 0
		nTasks     = 0
		stackPrune = Nil
	}
	
	override def setup(l: CPPropagStrength) : CPOutcome =  {
		
		// TODO : Generate the set of relevant activities 
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- 0 until tasks.size) {
        		if (!tasks(i).getStart.isBound) tasks(i).getStart.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getEnd.isBound) tasks(i).getEnd.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getDur.isBound) tasks(i).getDur.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getResource.isBound) tasks(i).getResource.callPropagateWhenBoundsChange(this)
        		// TODO Pierre : is it registered only for the specified value ?
        		if (!tasks(i).getMachines.isBound) tasks(i).getMachines.callValRemoveIdxWhenValueIsRemoved(this, r)
        	}
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
		
		var modified = true
		var res : Tuple2[Boolean, CPOutcome] = null
		
		while (modified) {
				
			res = sweepAlgorithm
			modified = res._1
			if (res._2 == CPOutcome.Failure) return CPOutcome.Failure
		}
        
		return CPOutcome.Suspend
	}
	
	def generateEventPointSeries : Boolean = {
		
		// Reset eventPointSeries
		eventPointSeries.clear
		
		// 
		var profileEvent = false
		
		// For each cumulative activity
		for (i <- 0 until tasks.size) {
			
			if (tasks(i).hasCompulsoryPart && tasks(i).getMachines.isBoundTo(r)) {
				
				// Check
				if (tasks(i).getMaxResource < max(0, lowerBound)) { // TODO
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).getLST, 1)
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).getECT, -1)
				}
				
				// Profile (Bad)
				if (tasks(i).getMaxResource < 0) { // TODO
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getLST, tasks(i).getMaxResource())  // TODO
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getECT, -tasks(i).getMaxResource()) // TODO
				}			
			}
			
			if (tasks(i).getMachines.hasValue(r)) {
				
				// Profile (Good)
				if (tasks(i).getMaxResource > 0) { // TODO
					
					// Generate events		
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getEST, tasks(i).getMaxResource())  // TODO
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getLCT, -tasks(i).getMaxResource()) // TODO
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).getStart.isBound && tasks(i).getEnd.isBound && tasks(i).getMachines.isBoundTo(r) && tasks(i).getResource.isBound)) {
					
					// Generate event
					eventPointSeries enqueue new Event(EventType.Pruning, i, tasks(i).getEST, 0)
				}
			}			
		}
		
		// TODO : switch
		//profileEvent
		true
	}
	
	def nextEvent = if (eventPointSeries.size > 0) eventPointSeries.dequeue else null
	
	def sweepAlgorithm : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		// Reset the parameters of the sweep line
		resetSweepLine
		
		// Generate events (no need to sort them as we use a priorityQueue)
		if (!generateEventPointSeries) CPOutcome.Suspend
		
		var event = nextEvent
		var d = event.date
		
		while (event != null) {
		
			if (!event.isPruningEvent) {
				
				// If we have considered all the events of the previous date
				if (d != event.date) {
					// Consistency check
					if (nTasks > 0 && sumHeight < lowerBound) return (change, CPOutcome.Failure) // TODO
					// Pruning (this will empty the stackPrune list)
					res = prune(d, event.date - 1)
					change |= res._1
					if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
					// New date to consider
					d = event.date	
				}
				
				if (event.isCheckEvent)
					nTasks += event.increment
				else if (event.isProfileEvent)
					sumHeight += event.increment
			}
			else {
				stackPrune = event.task :: stackPrune
			}
			
			event = nextEvent
		}
		
		// Consistency check
		if (nTasks > 0 && sumHeight < lowerBound) return (change, CPOutcome.Failure) // TODO
		// Pruning
		res = prune(d, d)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		return (change, CPOutcome.Suspend)
	}
	
	def prune(low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		for (i <- 0 until stackPrune.size) {
			
			res = pruneMandatory(tasks(stackPrune(i)), low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = pruneForbiden(tasks(stackPrune(i)), low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = pruneConsumption(tasks(stackPrune(i)), low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		}	
		
		return (change, CPOutcome.Suspend)
	}
	
	def pruneMandatory(t : CumulativeActivity, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		// Consistency check
		if (nTasks == 0 || (sumHeight - t.getMaxResource) >= lowerBound) { // TODO
			return (change, CPOutcome.Suspend)
		}
		
		// Fix the activity to the machine r and check consistency
		res = MultiCumulative.fixVar(t.getMachines, r)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the EST of the activity and check consistency
		res = MultiCumulative.adjustMin(t.getStart, up - t.getMaxDuration + 1)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the LST of the activity and check consistency
		res = MultiCumulative.adjustMax(t.getStart, low)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the LCT of the activity and check consistency
		res = MultiCumulative.adjustMax(t.getEnd, low + t.getMaxDuration)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the ECT of the activity and check consistency
		res = MultiCumulative.adjustMin(t.getEnd, up + 1)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the minimal duration of the activity and check consistency
		res = MultiCumulative.adjustMin(t.getDur, min(up - t.getLST+1, t.getECT-low))
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
		return (change, CPOutcome.Suspend)
	}
	
	def pruneForbiden(t : CumulativeActivity, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		
		//TODO
			
		return (change, CPOutcome.Suspend)
	}
	
	def pruneConsumption(t : CumulativeActivity, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var res : Tuple2[Boolean, CPOutcome] = (false, CPOutcome.Suspend)
		
		if (t.getMachines.isBoundTo(r) && t.getECT > low && t.getLST <= up && t.getMinDuration > 0) {
			
			res = MultiCumulative.adjustMin(t.getResource, lowerBound - (sumHeight - t.getMaxResource)) // TODO
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