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
 */
class MultiCumulative (cp: CPSolver, tasks : Array[CumulativeActivity], limits : Array[Int]) extends Constraint(tasks(0).getMachines.getStore(), "MultiCumulative") {
	
	// Event Point Series
	val eventPointSeries = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = b.date - a.date })
	val sweepLine : SweepLine = new SweepLine
	
	override def setup(l: CPPropagStrength) : CPOutcome =  {
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- 0 until tasks.size) {
        		if (!tasks(i).getStart.isBound) tasks(i).getStart.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getEnd.isBound) tasks(i).getEnd.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getDur.isBound) tasks(i).getDur.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getResource.isBound) tasks(i).getResource.callPropagateWhenBoundsChange(this)
        		if (!tasks(i).getMachines.isBound) tasks(i).getMachines.callPropagateWhenDomainChanges(this)
        	}
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
		
		var change = true
		var res : Tuple2[Boolean, CPOutcome] = null
		
		for (i <- 0 until limits.size) {
			
			while (change) {
				
				res = sweepAlgorithm(i)
				change = res._1
				if (res._2 == CPOutcome.Failure) return CPOutcome.Failure
			}
		}
        
		return CPOutcome.Suspend
	}
	
	def nextEvent = if (eventPointSeries.size > 0) eventPointSeries.dequeue else null
	
	def generateEventPointSeries(r : Int) {
		
		// Reset eventPointSeries
		eventPointSeries.clear
		
		// For each cumulative activity
		for (i <- 0 until tasks.size) {
			
			if (tasks(i).hasCompulsoryPart && tasks(i).getMachines.isBoundTo(r)) {
				
				// Check
				if (tasks(i).getMaxResource < max(0, limits(r))) {
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Check, tasks(i), tasks(i).getLST, 1)
					eventPointSeries enqueue new Event(EventType.Check, tasks(i), tasks(i).getECT, -1)
				}
				
				// Profile (Bad)
				if (tasks(i).getMaxResource < 0) {
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Profile, tasks(i), tasks(i).getLST, tasks(i).getMaxResource())
					eventPointSeries enqueue new Event(EventType.Profile, tasks(i), tasks(i).getECT, -tasks(i).getMaxResource())
				}			
			}
			
			if (tasks(i).getMachines.hasValue(r)) {
				
				// Profile (Good)
				if (tasks(i).getMaxResource > 0) {
					
					// Generate events		
					eventPointSeries enqueue new Event(EventType.Profile, tasks(i), tasks(i).getEST, tasks(i).getMaxResource())
					eventPointSeries enqueue new Event(EventType.Profile, tasks(i), tasks(i).getLCT, -tasks(i).getMaxResource())
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).getStart.isBound && tasks(i).getEnd.isBound && tasks(i).getMachines.isBound && tasks(i).getResource.isBound)) {
					
					// Generate event
					eventPointSeries enqueue new Event(EventType.Pruning, tasks(i), tasks(i).getEST, 0)
				}
			}			
		}
	}

	def sweepAlgorithm(r : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		// Reset the parameters of the sweep line
		sweepLine.reset
		
		// Generate events (no need to sort them as we use a priorityQueue)
		generateEventPointSeries(r)
		
		var event = nextEvent
		var d = event.date
		
		while (event != null) {
		
			if (!event.isPruningEvent) {
				
				// If we have considered all the events of the previous date
				if (d != event.date) {
					// Consistency check
					if (sweepLine.nTasks > 0 && sweepLine.sumHeight < limits(r)) return (change, CPOutcome.Failure)
					// Pruning (this will empty the stackPrune list)
					res = prune(r, d, event.date - 1)
					change |= res._1
					if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
					// New date to consider
					d = event.date	
				}
				
				if (event.isCheckEvent)
					sweepLine.nTasks += event.increment
				else if (event.isProfileEvent)
					sweepLine.sumHeight += event.increment
			}
			else {
				sweepLine.stackPrune = event.task :: sweepLine.stackPrune
			}
			
			event = nextEvent
		}
		
		// Consistency check
		if (sweepLine.nTasks > 0 && sweepLine.sumHeight < limits(r)) return (change, CPOutcome.Failure)
		// Pruning
		res = prune(r, d, d)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		return (change, CPOutcome.Suspend)
	}
	
	def prune(r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		for (i <- 0 until sweepLine.stackPrune.size) {
			
			res = pruneMandatory(sweepLine.stackPrune(i), r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = pruneForbiden(sweepLine.stackPrune(i), r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = pruneConsumption(sweepLine.stackPrune(i), r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		}	
		
		return (change, CPOutcome.Suspend)
	}
	
	def pruneMandatory(t : CumulativeActivity, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		// Consistency check
		if (sweepLine.nTasks == 0 || (sweepLine.sumHeight - t.getMaxResource) >= limits(r)) {
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
	
	def pruneForbiden(t : CumulativeActivity, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		
		//TODO
			
		return (change, CPOutcome.Suspend)
	}
	
	def pruneConsumption(t : CumulativeActivity, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var res : Tuple2[Boolean, CPOutcome] = (false, CPOutcome.Suspend)
		
		if (t.getMachines.isBound && t.getECT > low && t.getLST <= up && t.getMinDuration > 0) {
			
			res = MultiCumulative.adjustMin(t.getResource, limits(r) - (sweepLine.sumHeight - t.getMaxResource))
		}
			
		return res
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
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + inc +">" }
	}
}

object MultiCumulative extends CPModel {
	
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
	
	def main(args: Array[String]) {
		
		val cp = CPSolver()
		
		val t1 = new CumulativeActivity(new CPVarInt(cp, 1 to 2), // start
										new CPVarInt(cp, 2 to 4), // duration
										new CPVarInt(cp, 3 to 6), // end
										new CPVarInt(cp, 0 to 0), // machine
										new CPVarInt(cp, -1 to 1)) // resource
		
		val t2 = new CumulativeActivity(new CPVarInt(cp, 0 to 6), // start
										new CPVarInt(cp, 0 to 2), // duration
										new CPVarInt(cp, 0 to 8), // end
										new CPVarInt(cp, 0 to 1), // machine
										new CPVarInt(cp, -3 to 4)) // resource
		
		val tasks = Array(t1, t2)
		val capacities = Array(4, 3)
		
		val constraint = new MultiCumulative(cp, tasks, capacities)
		
		constraint.propagate
		
		println(t2.getStart)
		println(t2.getEnd)
		println(t2.getDur)
		println(t2.getResource)
	}
}