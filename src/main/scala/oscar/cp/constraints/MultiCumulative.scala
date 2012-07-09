package oscar.algo

import scala.math.max
import scala.collection.immutable.Queue

import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.modeling.CPSolver
import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength

/**
 * 
 */
class MultiCumulative (cp: CPSolver, tasks : Array[CumulativeActivity], limits : Array[Int]) extends Constraint(tasks(0).mach.getStore(), "MultiCumulative") {
	
	// Event Point Series
	private var eventPointSeries = Queue[Event]()
	private val sweepLine : SweepLine = new SweepLine
	
	override def setup(l: CPPropagStrength) : CPOutcome =  {
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
                //if (!X.isBound()) X.callPropagateWhenMinChanges(this)
                //if (!Y.isBound()) Y.callPropagateWhenMaxChanges(this)
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
	  
		// Algorithm
        
		return CPOutcome.Suspend
	}

	def extractEvent : Event = { 
		
		val e = eventPointSeries.head 
		eventPointSeries = eventPointSeries.tail
		return e
	}
	
	
	def generateEventPointSeries(r : Int) {
		
		// For each cumulative activity
		for (i <- 0 until tasks.size) {
			
			if (tasks(i).hasCompulsoryPart && tasks(i).mach.isBoundTo(r)) {
				
				// Check
				if (tasks(i).getMaxResource < max(0, limits(r))) {
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Check, tasks(i), tasks(i).getLST, 1)
					eventPointSeries enqueue new Event(EventType.Check, tasks(i), tasks(i).getECT, -1)
				}
				
				// Profile Bad
				if (tasks(i).getMaxResource < 0) {
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Profile, tasks(i), tasks(i).getLST, tasks(i).getMaxResource())
					eventPointSeries enqueue new Event(EventType.Profile, tasks(i), tasks(i).getECT, -tasks(i).getMaxResource())
				}			
			}
			
			if (tasks(i).mach.hasValue(r)) {
				
				// Profile Good
				if (tasks(i).getMaxResource > 0) {
					
					// Generate events		
					eventPointSeries enqueue new Event(EventType.Profile, tasks(i), tasks(i).getEST, tasks(i).getMaxResource())
					eventPointSeries enqueue new Event(EventType.Profile, tasks(i), tasks(i).getLCT, -tasks(i).getMaxResource())
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).getStart.isBound && tasks(i).getEnd.isBound && tasks(i).mach.isBound && tasks(i).getResource.isBound)) {
					
					// Generate event
					eventPointSeries enqueue new Event(EventType.Pruning, tasks(i), tasks(i).getEST, 0)
				}
			}			
		}
	}

	def sweepAlgorithm(r : Int) = {
		
		sweepLine.reset
		
		var event = extractEvent
		var d = event.date
		
		while (event != Nil) {
		
			if (!event.isPruningEvent) {
				
				if (d != event.date) {
					if (sweepLine.nTasks > 0 && sweepLine.sumHeight < limits(r)) CPOutcome.Failure
					//Prune
					prune
					d = event.date	
				}
				
				if (event.isCheckEvent)
					sweepLine.nTasks += event.increment
				else if (event.isProfileEvent) // event can have an unknown type
					sweepLine.sumHeight += event.increment
			}
			else {
				sweepLine.stackPrune = event.task :: sweepLine.stackPrune
			}
			
			event = extractEvent
		}
		
		if (sweepLine.nTasks > 0 && sweepLine.sumHeight < limits(r)) CPOutcome.Failure	
		//Prune
		prune
	}
	
	def prune = {
		
		pruneMandatory
		
	}
	
	def pruneMandatory = {
		
		
	}
	
	def pruneIntervals = {
		
		
	}
	
	def pruneFixed = {
		
		
	}
	
	/** The Sweep Line
	 */
	class SweepLine() {
		
		var sumHeight  : Int = _ //Size : number of machines
		var nTasks     : Int = _ //Size : number of machines
		var stackPrune : List[CumulativeActivity] = Nil
		
		def reset = {
			
			sumHeight  = 0
			nTasks     = 0
			stackPrune = Nil
		}
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
	
	class Event(e : EventType, t : CumulativeActivity, d : Int, inc : Int) extends Enumeration {

		def isCheckEvent   = { e == EventType.Check }
		def isProfileEvent = { e == EventType.Profile }
		def isPruningEvent = { e == EventType.Pruning }
		
		def date      = d
		def eType     = e
		def increment = inc
		def task      = t
	}
}