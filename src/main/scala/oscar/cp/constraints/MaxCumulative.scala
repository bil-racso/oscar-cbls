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
 * This class implements the cumulative constraint described in [1]. 
 * 
 * @param:
 * - cp : the CPSolver linked to the 
 * - allTasks :
 * - limit :
 * - r : 
 *
 * 
 * 
 *
 * @authors: Renaud Hartert ren.hatert@gmail.com
 * 
 * @references:
 * - [1] A New Multi-Resource cumulatives Constraint with Negative Heights,
 *       Nicolas Beldiceanu and Mats Carlsson   
 * - [2] Choco'class CumulSweep.java
 */

class MaxCumulative (cp: CPSolver, allTasks : Array[CumulativeActivity], limit : Int, r : Int) extends Constraint(cp, "MaxCumulative") {

	// Keeps only the relevant tasks
	val tasks = allTasks.filter(_.getMachines.hasValue(r))
	
	val nTasks = tasks.size
	val Tasks  = 0 until nTasks
	
	// Event Point Series (min heap on date)
	val eventPointSeries = new PriorityQueue[Event]()(new Ordering[Event] { def compare(a : Event, b : Event) = if (b.date > a.date) {1} else if (b.date == a.date) {0} else {-1} })
	
	// Sweep line parameters
	var delta      : Int = 0
	var sumHeight  : Int = 0
	val stackPrune : Set[Int] = Set()

	// Capacities added to sumHeight during a sweep
	val contribution = new Array[Int](nTasks)
	
	// Preprocessed events used to increase efficiency of the constraint
	val eventList = Array.tabulate(nTasks){e => new EventList(e)}

	override def setup(l: CPPropagStrength) : CPOutcome = {
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- Tasks) {
        		
        		tasks(i).getStart.callPropagateWhenBoundsChange(this)
		        tasks(i).getDur.callPropagateWhenBoundsChange(this)
		        tasks(i).getEnd.callPropagateWhenBoundsChange(this)
		        tasks(i).getResource.callPropagateWhenBoundsChange(this)
		        tasks(i).getMachines.callPropagateWhenDomainChanges(this)
        	}
        }
        
        return oc      
  	}
  
	override def propagate(): CPOutcome = {
			
		if (sweepAlgorithm == CPOutcome.Failure) return CPOutcome.Failure
        
		return CPOutcome.Suspend
	}
	
	def nextEvent = if (eventPointSeries.size > 0) eventPointSeries.dequeue else null
	
	def generateEventPointSeries : Boolean = {
		
		// True if a profile event has been generated
		var profileEvent = false
		
		// Reset eventPointSeries
		eventPointSeries.clear
		
		for (i <- Tasks) {
			
			if (tasks(i).getLST < tasks(i).getECT && tasks(i).getMachines.isBoundTo(r)) {
				
				// Profile (Bad : on compulsory part)
				if (tasks(i).getMinResource > 0) {
					
					// Generates events
					eventPointSeries enqueue eventList(i).getSBadProfile
					eventPointSeries enqueue eventList(i).getEBadProfile

					profileEvent = true
				}			
			}
			
			if (tasks(i).getMachines.hasValue(r)) {
				
				// Profile (Good : on entire domain)
				if (tasks(i).getMinResource < 0) {
					
					// Generates events		
					eventPointSeries enqueue eventList(i).getSGoodProfile
					eventPointSeries enqueue eventList(i).getEGoodProfile
					
					profileEvent = true
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).getStart.isBound && tasks(i).getEnd.isBound && tasks(i).getMachines.isBoundTo(r) && tasks(i).getResource.isBound)) {
					
					// Generates event
					eventPointSeries enqueue eventList(i).getPruning
				}
			}			
		}
		
		profileEvent	
	}
	
	def resetSweepLine = {
			
		delta     = 0
		sumHeight = 0
		stackPrune.clear
			
		for (i <- Tasks) {
			contribution(i) = 0
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
		
		while(event != null) {
		
			if (!event.isPruningEvent) {
				
				// If we have considered all the events of the previous date
				if (delta != event.date) {
					
					// Consistency check
					if (sumHeight > limit) 
						return CPOutcome.Failure
					
					// Pruning (this will empty the stackPrune list)
					if (prune(r, delta, event.date - 1) == CPOutcome.Failure) 
						return CPOutcome.Failure
						
					// New date to consider
					delta = event.date	
				}
				
				if (event.isProfileEvent) {
					
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
		if (sumHeight > limit) 
			return CPOutcome.Failure
			
		// Pruning
		if (prune(r, delta, Int.MaxValue) == CPOutcome.Failure) 
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
		if ((sumHeight - contribution(t)) <= limit) {
			return CPOutcome.Suspend
		}
		
		// Fix the activity to the machine r
		if (tasks(t).getMachines.assign(r) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the EST of the activity
		if (tasks(t).getStart.updateMin(up - tasks(t).getMaxDuration + 1) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the LST of the activity
		if (tasks(t).getStart.updateMax(low) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the LCT of the activity
		if (tasks(t).getEnd.updateMax(low + tasks(t).getMaxDuration) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the ECT of the activity
		if (tasks(t).getEnd.updateMin(up + 1) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the minimal duration of the activity
		if (tasks(t).getDur.updateMin(min(up - tasks(t).getLST+1, tasks(t).getECT-low)) == CPOutcome.Failure) 
			return CPOutcome.Failure
			
		return CPOutcome.Suspend
	}
	
	def pruneForbiden(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (sumHeight - contribution(t) + tasks(t).getMinResource > limit) {
			
			if (tasks(t).getECT > low && tasks(t).getLST <= up && tasks(t).getMinDuration > 0) {
					
				if (tasks(t).getMachines.removeValue(r) == CPOutcome.Failure) 
					return CPOutcome.Failure
				
			} else if (tasks(t).getMachines.isBoundTo(r)) {
				
				if (tasks(t).getMinDuration > 0) {
					
					//INTERVAL PRUNING
					for (i <- low - tasks(t).getMinDuration+1 to up) {
						
						if (tasks(t).getStart.removeValue(i) == CPOutcome.Failure) 
							return CPOutcome.Failure
					}
					
					for (i <- low + 1 to up + tasks(t).getMinDuration) {
						
						if (tasks(t).getEnd.removeValue(i) == CPOutcome.Failure) 
							return CPOutcome.Failure
					}
				}
				
				val maxD = max(max(low - tasks(t).getEST, tasks(t).getLCT -up - 1), 0)
				
				if (tasks(t).getDur.updateMax(maxD) == CPOutcome.Failure) 
					return CPOutcome.Failure
			}
		}
			
		return CPOutcome.Suspend
	}
	
	def pruneConsumption(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (tasks(t).getMachines.isBoundTo(r) && tasks(t).getECT > low && tasks(t).getLST <= up && tasks(t).getMinDuration > 0) {
			
			if (tasks(t).getResource.updateMax(limit - (sumHeight - contribution(t))) == CPOutcome.Failure) 
				return CPOutcome.Failure
		}
			
		return CPOutcome.Suspend
	}
	
	object EventType extends Enumeration {
		
		type EventType = Value
		
		val Check   = Value("Check")
		val Profile = Value("Profile")
		val Pruning = Value("Pruning")
	}
	
	import EventType._
	
	class Event(e : EventType, t : Int, private var d : Int, private var inc : Int) extends Enumeration {

		def isCheckEvent   = { e == EventType.Check }
		def isProfileEvent = { e == EventType.Profile }
		def isPruningEvent = { e == EventType.Pruning }
		
		def date = d		
		def increment = inc
		def eType     = e
		def task      = t
		
		def date_= (x : Int) {d = x}
		def increment_= (x : Int) {inc = x}
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + inc +">" }
	}
	
	class EventList(t : Int) {
		
		val sCheck       : Event = new Event(EventType.Check, t, 0, 1)
		val eCheck       : Event = new Event(EventType.Check, t, 0, -1)
		val sBadProfile  : Event = new Event(EventType.Profile, t, 0, 0)
		val eBadProfile  : Event = new Event(EventType.Profile, t, 0, 0)
		val sGoodProfile : Event = new Event(EventType.Profile, t, 0, 0)
		val eGoodProfile : Event = new Event(EventType.Profile, t, 0, 0)
		val Pruning      : Event = new Event(EventType.Pruning, t, 0, 0)
		
		def getSCheck : Event = {
			sCheck.date = tasks(sCheck.task).getLST
			return sCheck
		}
		
		def getECheck : Event = {
			eCheck.date = tasks(eCheck.task).getECT
			return eCheck
		}
		
		def getSBadProfile : Event = {
			sBadProfile.date = tasks(sBadProfile.task).getLST
			sBadProfile.increment = tasks(sBadProfile.task).getMinResource
			return sBadProfile
		}
		
		def getEBadProfile : Event = {
			eBadProfile.date = tasks(eBadProfile.task).getECT
			eBadProfile.increment = -tasks(eBadProfile.task).getMinResource
			return eBadProfile
		}
		
		def getSGoodProfile : Event = {
			sGoodProfile.date = tasks(sGoodProfile.task).getEST
			sGoodProfile.increment = tasks(sGoodProfile.task).getMinResource
			return sGoodProfile
		}
		
		def getEGoodProfile : Event = {
			eGoodProfile.date = tasks(eGoodProfile.task).getLCT
			eGoodProfile.increment = -tasks(eGoodProfile.task).getMinResource
			return eGoodProfile
		}
		
		def getPruning : Event = {
			Pruning.date = tasks(Pruning.task).getEST
			return Pruning
		}
	}
}