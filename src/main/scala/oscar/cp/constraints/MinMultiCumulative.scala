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
class MinMultiCumulative (cp: CPSolver, tasks : Array[CumulativeActivity], lowerBound : Array[Int], upperBound : Array[Int], minCumulative : Boolean) extends Constraint(tasks(0).getMachines.getStore(), "MinMultiCumulative") {
	
	// Event Point Series
	val eventPointSeries = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = b.date - a.date })
	
	val consContribution = new Array[Int](tasks.size)
	val capaContribution = new Array[Int](tasks.size)
	
	// Sweep line
	var consSumHeight  : Int = 0
	var capaSumHeight  : Int = 0
	var nTasks     : Int = 0
	var stackPrune : List[Int] = Nil
		
	def reset = {
			
		consSumHeight  = 0
		nTasks     = 0
		stackPrune = Nil
			
		for (i <- 0 until tasks.size) {
			consContribution(i) = 0
			capaContribution(i) = 0
		}
	}
	
	val toPropagate = new Array[Boolean](lowerBound.size)
	
	override def setup(l: CPPropagStrength) : CPOutcome =  {
		
		for (i <- 0 until lowerBound.size)
			toPropagate(i) = true
		
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
		
		for (i <- 0 until lowerBound.size; if (toPropagate(i))) {
			
			//toPropagate(i) = false
			
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
				if (tasks(i).getMaxResource < max(0, lowerBound(r))) {
					
					// Generate events
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).getLST, 1, 1)
					eventPointSeries enqueue new Event(EventType.Check, i, tasks(i).getECT, -1, -1)
				}
				
				// Profile (Bad)
				val cons = min(0, tasks(i).getMaxResource)
				val capa = max(0, tasks(i).getMinResource)
				
				//if (tasks(i).getMaxResource < 0) {
				if (cons != 0 || capa != 0) {
					// Generate events
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getLST, cons, capa)  
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getECT, -cons, -capa) 
				}			
			}
			
			if (tasks(i).getMachines.hasValue(r)) {
				
				// Profile (Good)
				val cons = max(0, tasks(i).getMaxResource)
				val capa = min(0, tasks(i).getMinResource)
				
				//if (tasks(i).getMaxResource > 0) { 
				if (cons != 0 || capa != 0) {
					// Generate events	
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getEST, cons, capa)  
					eventPointSeries enqueue new Event(EventType.Profile, i, tasks(i).getLCT, -cons, -capa)
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).getStart.isBound && tasks(i).getEnd.isBound && tasks(i).getMachines.isBoundTo(r) && tasks(i).getResource.isBound)) {
					
					// Generate event
					eventPointSeries enqueue new Event(EventType.Pruning, i, tasks(i).getEST, 0, 0)
				}
			}			
		}
	}

	def sweepAlgorithm(r : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		// Reset the parameters of the sweep line
		reset
		
		// Generate events (no need to sort them as we use a priorityQueue)
		generateEventPointSeries(r)
		
		var event = nextEvent
		var d = event.date
		
		while (event != null) {
		
			if (!event.isPruningEvent) {
				
				// If we have considered all the events of the previous date
				if (d != event.date) {
					// Consistency check
					if ((nTasks > 0 && consSumHeight < lowerBound(r)) || capaSumHeight > upperBound(r)) return (change, CPOutcome.Failure)
					// Pruning (this will empty the stackPrune list)
					res = prune(r, d, event.date - 1)
					change |= res._1
					if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
					// New date to consider
					d = event.date	
				}
				
				if (event.isCheckEvent)
					nTasks += event.capaInc
				else if (event.isProfileEvent) {
					
					consSumHeight += event.consInc
					consContribution(event.task) += event.consInc
					
					capaSumHeight += event.capaInc
					capaContribution(event.task) += event.capaInc
				}
			}
			else {
				stackPrune = event.task :: stackPrune
			}
			
			event = nextEvent
		}
		
		// Consistency check
		if ((nTasks > 0 && consSumHeight < lowerBound(r)) || capaSumHeight > upperBound(r)) return (change, CPOutcome.Failure)
		// Pruning
		res = prune(r, d, d)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		return (change, CPOutcome.Suspend)
	}
	
	def prune(r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		for (i <- 0 until stackPrune.size) {
			
			res = pruneMandatory(stackPrune(i), r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = pruneForbiden(stackPrune(i), r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = pruneConsumption(stackPrune(i), r, low, up)
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		}	
		
		return (change, CPOutcome.Suspend)
	}
	
	def pruneMandatory(t : Int, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		// Consistency check
		if (nTasks == 0 || (consSumHeight - consContribution(t)) >= lowerBound(r) 
				  		&& (capaSumHeight - capaContribution(t)) <= upperBound(r)) {
			return (change, CPOutcome.Suspend)
		}
		
		// Fix the activity to the machine r and check consistency
		res = MinMultiCumulative.fixVar(tasks(t).getMachines, r)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the EST of the activity and check consistency
		res = MinMultiCumulative.adjustMin(tasks(t).getStart, up - tasks(t).getMaxDuration + 1)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the LST of the activity and check consistency
		res = MinMultiCumulative.adjustMax(tasks(t).getStart, low)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the LCT of the activity and check consistency
		res = MinMultiCumulative.adjustMax(tasks(t).getEnd, low + tasks(t).getMaxDuration)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the ECT of the activity and check consistency
		res = MinMultiCumulative.adjustMin(tasks(t).getEnd, up + 1)
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		
		// Adjust the minimal duration of the activity and check consistency
		res = MinMultiCumulative.adjustMin(tasks(t).getDur, min(up - tasks(t).getLST+1, tasks(t).getECT-low))
		change |= res._1
		if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
		return (change, CPOutcome.Suspend)
	}
	
	def pruneForbiden(t : Int, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		
		//TODO
			
		return (change, CPOutcome.Suspend)
	}
	
	def pruneConsumption(t : Int, r : Int, low : Int, up : Int) : Tuple2[Boolean, CPOutcome] = {
		
		var change = false
		var res : Tuple2[Boolean, CPOutcome] = null
		
		if (tasks(t).getMachines.isBoundTo(r) && tasks(t).getECT > low && tasks(t).getLST <= up && tasks(t).getMinDuration > 0) {
			
			res = MinMultiCumulative.adjustMin(tasks(t).getResource, lowerBound(r) - (consSumHeight - consContribution(t)))
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
			
			res = MinMultiCumulative.adjustMax(tasks(t).getResource, upperBound(r) - (capaSumHeight - capaContribution(t)))
			change |= res._1
			if (res._2 == CPOutcome.Failure) return (change, CPOutcome.Failure)
		}
			
		return (change, CPOutcome.Suspend)
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
	
	class Event(e : EventType, t : Int, d : Int, cons : Int, capa : Int) extends Enumeration {

		def isCheckEvent   = { e == EventType.Check }
		def isProfileEvent = { e == EventType.Profile }
		def isPruningEvent = { e == EventType.Pruning }
		
		def date    = d
		def eType   = e
		def consInc = cons
		def capaInc = capa
		def task    = t
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + cons + "," + capa +">" }
	}
}

object MinMultiCumulative extends CPModel {
	
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