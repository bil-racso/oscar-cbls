package oscar.cp.constraints

import scala.math.max
import scala.math.min
import scala.collection.mutable.Set

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.scheduling.SortUtils.stableSort
import oscar.cp.modeling.CPSolver

abstract class SweepCumulativeA (cp: CPSolver, allTasks : Array[CumulativeActivity], lb : Int, ub : Int, r : Int, name : String) extends Constraint(allTasks(0).machine.getStore(), name) {

	// Keeps only the relevant tasks
	protected val tasks = allTasks.filter(_.machine.hasValue(r))
	
	protected val nTasks = tasks.size
	protected val Tasks  = 0 until nTasks
	
	// Event Point Series
	protected val eventPointSeries : Array[Event]//= new Array[Event](nTasks*7)
	protected var nEvents = 0
	
	// Sweep line parameters
	protected var delta         : Int = 0	
	protected var consSumHeight : Int = 0
	protected var capaSumHeight : Int = 0
	protected var nCurrentTasks : Int = 0
	protected val stackPrune : Set[Int] = Set()	

	// Capacities added to during a sweep
	protected val consContrib = new Array[Int](nTasks)
	protected val capaContrib = new Array[Int](nTasks)
	
	// Preprocessed events used to increase efficiency of the constraint
	protected val eventList = Array.tabulate(nTasks){e => new EventList(e)}
	
	override def setup(l: CPPropagStrength) : CPOutcome = {
	
		setPriorityL2(0)
		
        val oc = propagate()
        
        if (oc == CPOutcome.Suspend) {
        	for (i <- Tasks) {
        		if (!tasks(i).start.isBound) tasks(i).start.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).dur.isBound) tasks(i).dur.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).end.isBound) tasks(i).end.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).resource.isBound) tasks(i).resource.callPropagateWhenBoundsChange(this)
	        	if (!tasks(i).machine.isBound) tasks(i).machine.callPropagateWhenDomainChanges(this)
    		}
        }
        
        return oc   
  	}
  
	override def propagate(): CPOutcome = {
		
		// Generate events
		if (!generateEventPointSeries()) 
			return CPOutcome.Suspend
			
		// Perform a sweep on the events
		if (sweepAlgorithm() == CPOutcome.Failure) 
			return CPOutcome.Failure
        
		return CPOutcome.Suspend
	}
	
	private def generateEventPointSeries() : Boolean = {
		
		// True if a profile event has been generated
		var profileEvent = false
		
		// Reset eventPointSeries
		nEvents = 0
		
		for (i <- Tasks) {
			
			if (tasks(i).lst < tasks(i).ect && tasks(i).machine.isBoundTo(r)) {
				
				// Check
				generateCheck(i)
				
				// Profile (Bad : on compulsory part)
				profileEvent |= generateProfileBad(i)		
			}
			
			if (tasks(i).machine.hasValue(r)) {
				
				// Profile (Good : on entire domain)
				profileEvent |= generateProfileGood(i)
				
				// Pruning (if something is not fixed)
				if (!tasks(i).start.isBound || !tasks(i).end.isBound || !tasks(i).machine.isBoundTo(r) || !tasks(i).resource.isBound) {
					
					// Generates event
					eventPointSeries(nEvents) = eventList(i).sPruning
					nEvents += 1
				}
			}			
		}
		
		profileEvent
	}
	
	protected def generateCheck(i : Int) 
	
	protected def generateProfileBad(i : Int) : Boolean
	
	protected def generateProfileGood(i : Int) : Boolean
	
	private def resetSweepLine = {
			
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
	
	def consistencyCheck : Boolean

	private def sweepAlgorithm() : CPOutcome = {
		
		resetSweepLine
		
		// Sort by increasing date
		stableSort(eventPointSeries, 0, nEvents, (a:Event,b:Event) => a.date < b.date)
		
		var delta = eventPointSeries(nEvents-1).date
		
		for (i <- 0 until nEvents) {
			
			val event = eventPointSeries(i)
		
			if (event.eType != EventType.Pruning) {
				
				// If we have considered all the events of the previous date
				if (delta != event.date) {
					
					// Consistency check
					if (consistencyCheck) 
						return CPOutcome.Failure
					
					// Pruning (this will empty the stackPrune list)
					if (prune(delta, event.date - 1) == CPOutcome.Failure) 
						return CPOutcome.Failure
						
					// New date to consider
					delta = event.date	
				}
				
				if (event.eType == EventType.Profile) {
					
					// Adjust resource consumption
					consSumHeight += event.cons
					consContrib(event.task) += event.cons
					
					// Adjust resource capacity
					capaSumHeight += event.capa
					capaContrib(event.task) += event.capa
					
				} else if (event.eType == EventType.Check) {
					
					// Number of overlaping tasks
					nCurrentTasks += event.cons
				} 
			}
			else {
				stackPrune add event.task
			}
		}
		
		// Consistency check
		if (consistencyCheck) 
			return CPOutcome.Failure
			
		// Pruning
		if (prune(delta, delta) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		return CPOutcome.Suspend
	}
	
	private def prune(low : Int, up : Int) : CPOutcome = {
		
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
	
	private def pruneMandatory(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		// Consistency check
		if (mandatoryCheck(t))
			return CPOutcome.Suspend
		
		// Fix the activity to the machine r
		if (tasks(t).machine.assign(r) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the EST of the activity
		if (tasks(t).start.updateMin(up - tasks(t).maxDuration + 1) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the LST of the activity
		if (tasks(t).start.updateMax(low) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the LCT of the activity
		if (tasks(t).end.updateMax(low + tasks(t).maxDuration) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the ECT of the activity
		if (tasks(t).end.updateMin(up + 1) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		// Adjust the minimal duration of the activity
		if (tasks(t).dur.updateMin(min(up - tasks(t).lst+1, tasks(t).ect-low)) == CPOutcome.Failure) 
			return CPOutcome.Failure
			
		return CPOutcome.Suspend
	}
	
	def mandatoryCheck(t : Int) : Boolean
	
	def forbidenCheck(t : Int) : Boolean
	
	private def pruneForbiden(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (forbidenCheck(t)) {
			
			if (tasks(t).ect > low && tasks(t).lst <= up && tasks(t).minDuration > 0) {
					
				if (tasks(t).machine.removeValue(r) == CPOutcome.Failure) 
					return CPOutcome.Failure
				
			} else if (tasks(t).machine.isBoundTo(r)) {
				
				if (tasks(t).minDuration > 0) {
					
					if (pruneInterval(low - tasks(t).minDuration+1, up, tasks(t).start) == CPOutcome.Failure)
						return CPOutcome.Failure
						
					if (pruneInterval(low + 1, up + tasks(t).minDuration, tasks(t).end) == CPOutcome.Failure)
							return CPOutcome.Failure
				}
				
				val maxD = max(max(low - tasks(t).est, tasks(t).lct -up - 1), 0)
						
				if (tasks(t).dur.updateMax(maxD) == CPOutcome.Failure) 
					return CPOutcome.Failure
			}
		}
			
		return CPOutcome.Suspend
	}
	
	private def pruneConsumption(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (tasks(t).machine.isBoundTo(r) && tasks(t).ect > low && tasks(t).lst <= up && tasks(t).minDuration > 0) {
			
			if (tasks(t).resource.updateMin(lb - (consSumHeight - consContrib(t))) == CPOutcome.Failure) 
				return CPOutcome.Failure	
				
			if (tasks(t).resource.updateMax(ub - (capaSumHeight - capaContrib(t))) == CPOutcome.Failure) 
				return CPOutcome.Failure			
		}
			
		return CPOutcome.Suspend
	}
	
	private def pruneInterval(low : Int, up : Int, v : CPVarInt) : CPOutcome = {
		
		for (i <- low to up)
			if (v.removeValue(i) == CPOutcome.Failure)
				return CPOutcome.Failure
		
		return CPOutcome.Suspend
	}
	
	// The different event's types
	object EventType extends Enumeration {
		
		type EventType = Value
		
		val Check   = Value("Check")
		val Profile = Value("Profile")
		val Pruning = Value("Pruning")
	}
	
	import EventType._
	
	// The event
	class Event(e : EventType, t : Int, private var d : Int, private var consomation : Int, private var capacity : Int) extends Enumeration {

		def date  = d
		def eType = e
		def cons  = consomation
		def capa  = capacity
		def task  = t
		
		def date_= (x : Int) {d = x}
		def cons_= (x : Int) {consomation = x}
		def capa_= (x : Int) {capacity = x}
		
		override def toString = { "<" + e + ", " + t + ", " + d + ", " + capa + ", " + cons + ">" }
	}
	
	// The event structure used to increase the speed of the algorithm.
	class EventList(t : Int) {
		
		val sCheckEv       : Event = new Event(EventType.Check, t, 0, 1, 1)
		val eCheckEv       : Event = new Event(EventType.Check, t, 0, -1, -1)
		val sBadProfileEv  : Event = new Event(EventType.Profile, t, 0, 0, 0)
		val eBadProfileEv  : Event = new Event(EventType.Profile, t, 0, 0, 0)
		val sGoodProfileEv : Event = new Event(EventType.Profile, t, 0, 0, 0)
		val eGoodProfileEv : Event = new Event(EventType.Profile, t, 0, 0, 0)
		val PruningEv      : Event = new Event(EventType.Pruning, t, 0, 0, 0)
		
		def sCheck : Event = {
			sCheckEv.date = tasks(sCheckEv.task).lst
			return sCheckEv
		}
		
		def eCheck : Event = {
			eCheckEv.date = tasks(eCheckEv.task).ect
			return eCheckEv
		}
		
		def sBadProfile(consInc : Int, capaInc : Int)   : Event = {
			sBadProfileEv.date = tasks(sBadProfileEv.task).lst
			sBadProfileEv.capa = capaInc
			sBadProfileEv.cons = consInc
			return sBadProfileEv
		}
		
		def eBadProfile(consInc : Int, capaInc : Int) : Event = {
			eBadProfileEv.date =  tasks(eBadProfileEv.task).ect
			eBadProfileEv.capa = -capaInc
			eBadProfileEv.cons = -consInc
			return eBadProfileEv
		}
		
		def sGoodProfile(consInc : Int, capaInc : Int) : Event = {
			sGoodProfileEv.date = tasks(sGoodProfileEv.task).est
			sGoodProfileEv.capa = capaInc
			sGoodProfileEv.cons = consInc
			return sGoodProfileEv
		}
		
		def eGoodProfile(consInc : Int, capaInc : Int) : Event = {
			eGoodProfileEv.date =  tasks(eGoodProfileEv.task).lct
			eGoodProfileEv.capa = -capaInc
			eGoodProfileEv.cons = -consInc
			return eGoodProfileEv
		}
		
		def sPruning : Event = {
			PruningEv.date = tasks(PruningEv.task).est
			return PruningEv
		}
	}
}