/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.cp.constraints

import scala.math.max
import scala.math.min
import scala.collection.mutable.Set

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPOutcome
import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.modeling.CPSolver
import oscar.algo.SortUtils.stableSort

/** This abstract class contains the main parts of the cumulative constraint described 
 *  in [1]. 
 * 
 *  The abstract methods in this class allow to specialize the constraint in a Max, Min
 *  or Bounded cumulative constraint.
 *  
 *  [1] A New Multi-Resource cumulative Constraint with Negative Heights, Nicolas Beldiceanu and Mats Carlsson   
 *	[2] Choco's class CumulSweep.java
 *
 *	@define originalTasks
 *	the tasks that could be originally assigned to the resource `r`
 *  @define idOfT
 *  The id `t` of the considered task in the array `tasks`.
 *  @define addEvent
 *  If the conditions are met, the events are added.
 *  
 *  @define badEvents
 *  
 *  Note: bad events are defined over the compulsory part of the task.
 *  
 *  @define goodEvents
 *  
 *  Note: good events are defined over the entire execution's domain of the task.
 *
 *  @author Renaud Hartert
 *  @version 28/07/2012 
 */
abstract class SweepCumulativeA (cp: Store, allTasks : Array[CumulativeActivity], lb : Int, ub : Int, r : Int, name : String) extends Constraint(cp, name) {

	// Contains all the relevant tasks
	protected val tasks = allTasks.filter(_.machine.hasValue(r))
	
	protected val nTasks = tasks.size
	protected val Tasks  = 0 until nTasks
	
	// Contains all the events representing the tasks (needs to be initialized)
	protected val eventPointSeries : Array[Event]
	// Current size of eventPointSeries
	protected var nEvents = 0
	
	// Current position of the sweep line
	protected var delta : Int = 0	
	// Sum of the height of the tasks that overlap the sweep line
	protected var consSumHeight : Int = 0
	// Sum of the height of the tasks that overlap the sweep line
	protected var capaSumHeight : Int = 0
	// Number of tasks that overlap the sweep line
	protected var nCurrentTasks : Int = 0
	// Tasks that could intersect the sweep line
	protected val stackPrune    : Set[Int] = Set()	

	// Contribution of all the tasks that are added to consSumHeight
	protected val consContrib = new Array[Int](nTasks)
	// Contribution of all the tasks that are added to capaSumHeight
	protected val capaContrib = new Array[Int](nTasks)

	// Contains all the possible events of each task (used for speed-up)
	protected val eventList = Array.tabulate(nTasks){e => new EventList(e)}
	
	/** Checks the necessary conditions to add the check events of the task `t` 
	 *  in the array `eventPointSeries` at the position `nEvents`. 
	 *  
	 *  $addEvent
	 * 
	 *  @param $idOfT
	 */
	protected def generateCheck(i : Int) 
	
	/** Checks the necessary conditions to add the bad profile events of the 
	 *  task `t` in the array `eventPointSeries` at the position `nEvents`. 
	 *  
	 *  $addEvent
	 *  
	 *  $badEvents
	 * 
	 *  @param $idOfT
	 */
	protected def generateProfileBad(i : Int) : Boolean
	
	/** Checks the necessary conditions to add the good profile events of the 
	 *  task `t` in the array `eventPointSeries` at the position `nEvents`. 
	 *  
	 *  $addEvent
	 *  
	 *  $goodEvents
	 * 
	 *  @param $idOfT
	 */
	protected def generateProfileGood(i : Int) : Boolean
	
	/** Checks the consistency of the total consumption of the resource `r` for 
	 *  the current position of the sweep line `delta`.
	 *  
	 *  For example, in the case of a Max cumulative:
	 *  {{{capaSumHeight > ub}}}
	 * 
	 *  @return `true` if the consumption exceeds the limit(s), `false` otherwise.
	 */
	protected def consistencyCheck : Boolean
	
	/** Checks that the contribution of the task `t` in the total consumption of 
	 *  the resource `r` at the current position of the sweep line `delta` is not 
	 *  mandatory to respect the consistency.
	 *  
	 *  For example, in the case of a Max cumulative:
	 *  {{{capaSumHeight - capaContrib(t) <= ub}}}
	 *  
	 *  @param $idOfT
	 * 
	 *  @return `true` if the tasks `t` is mandatory, `false` otherwise.
	 */
	protected def mandatoryCheck(t : Int) : Boolean
	
	/** Checks that the task `t` is inconsistent with the total consumption of the
	 *  resource `r` at the current position of the sweep line `delta` no matter 
	 *  the height of its consumption.
	 *  
	 *  For example, in the case of a Max cumulative:
	 *  {{{capaSumHeight - capaContrib(t) + tasks(t).minResource > ub}}}
	 *  
	 *  @param $idOfT
	 * 
	 *  @return `true` if the tasks `t` is inconsistent, `false` otherwise.
	 */
	protected def forbidenCheck(t : Int) : Boolean
	
	
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
		
		// Generates events
		if (!generateEventPointSeries()) 
			return CPOutcome.Suspend
			
		// Performs a sweep on the events
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
					
					eventPointSeries(nEvents) = eventList(i).sPruning
					nEvents += 1
				}
			}			
		}
		
		profileEvent
	}
	
	
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
	

	private def sweepAlgorithm() : CPOutcome = {
		
		resetSweepLine
		
		// Sort events by increasing date
		stableSort(eventPointSeries, 0, nEvents, (a:Event,b:Event) => a.date < b.date)
		
		// First position of the sweep line
		var delta = eventPointSeries(0).date
		
		for (i <- 0 until nEvents) {
			
			val event = eventPointSeries(i)
		
			if (event.eType != EventType.Pruning) {
				
				// If we have considered all the events at the previous position
				// of the sweep line
				if (delta != event.date) {
					
					// Consistency check
					if (consistencyCheck) 
						return CPOutcome.Failure
					
					// Pruning (this could reduce the size of stackPrune)
					if (prune(delta, event.date - 1) == CPOutcome.Failure) 
						return CPOutcome.Failure
						
					// Moves the sweep line
					delta = event.date	
				}
				
				if (event.eType == EventType.Profile) {
					
					// Adjusts resource consumption
					consSumHeight += event.cons
					consContrib(event.task) += event.cons
					
					// Adjusts resource capacity
					capaSumHeight += event.capa
					capaContrib(event.task) += event.capa
					
				} else if (event.eType == EventType.Check) {
					
					// Number of overlapping tasks
					nCurrentTasks += event.cons
				} 
			}
			else {
				stackPrune.add(event.task)
			}
		}
		
		// Checks consistency
		if (consistencyCheck) 
			return CPOutcome.Failure
			
		// Final pruning
		if (prune(delta, delta) == CPOutcome.Failure) 
			return CPOutcome.Failure
		
		return CPOutcome.Suspend
	}
	
	
	private def prune(low : Int, up : Int) : CPOutcome = {
		
		val it = stackPrune.iterator
		
		while (!it.isEmpty) {
			
			val t = it.next
			
			// Pruning on tasks that are mandatory to respect consistency
			if (pruneMandatory(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			// Pruning on tasks that must be discarded to respect consistency
			if (pruneForbiden(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			// Adjusts the resource's consumption of the tasks
			if (pruneConsumption(t, r, low, up) == CPOutcome.Failure) 
				return CPOutcome.Failure
			
			if (tasks(t).lct <= up + 1) {
				stackPrune.remove(t)
			}
		}	

		return CPOutcome.Suspend
	}
	
	
	private def pruneMandatory(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		// Checks if the task is mandatory to respect consistency
		if (!mandatoryCheck(t))
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
	
	
	private def pruneForbiden(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		// Checks if the task must be discarded to respect consistency
		if (forbidenCheck(t)) {
			
			if (tasks(t).ect > low && tasks(t).lst <= up && tasks(t).minDuration > 0) {
					
				if (tasks(t).machine.removeValue(r) == CPOutcome.Failure) 
					return CPOutcome.Failure
				
			} else if (tasks(t).machine.isBoundTo(r)) {
				
				if (tasks(t).minDuration > 0) {
					
					if (pruneInterval(low - tasks(t).minDuration+1, up, tasks(t).start) == CPOutcome.Failure)
						return CPOutcome.Failure	
				} 
				
				if (!tasks(t).dur.isBound) {
					
					if (pruneInterval(low + 1, up + tasks(t).minDuration, tasks(t).end) == CPOutcome.Failure)
						return CPOutcome.Failure
						
					val maxD = max(max(low - tasks(t).est, tasks(t).lct -up - 1), 0)
								
					if (tasks(t).dur.updateMax(maxD) == CPOutcome.Failure) 
						return CPOutcome.Failure
				}
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
	    
	    assert(low <= up)
		if (low <= v.min && up <= v.max) {   
		  v.updateMin(up+1)
		} else if (up >= v.max && low >= v.min) {
		  v.updateMax(low-1)
		} else CPOutcome.Suspend
		
		
	    /*
	    // create holes, not a good idea Renaud ;-)
		for (i <- low to up)
			if (v.removeValue(i) == CPOutcome.Failure)
				return CPOutcome.Failure
		*/
		return CPOutcome.Suspend
		
	}
	
	
	/**
	 * 
	 */
	protected object EventType extends Enumeration {
		
		type EventType = Value
		
		val Check   = Value("Check")
		val Profile = Value("Profile")
		val Pruning = Value("Pruning")
	}
	
	
	import EventType._
	
	/**
	 * 
	 */
	protected class Event(e : EventType, t : Int, private var d : Int, private var consomation : Int, private var capacity : Int) extends Enumeration {

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
	
	/**
	 * 
	 */
	protected class EventList(t : Int) {
		
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