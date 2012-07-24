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
 * MaxCumulative
 * This constraint assures that, for each instant t, the amount of resource consumed by 
 * the tasks executed on the machine r will never exceed the capacity of this machine.
 * 
 * @param:
 * - cp       : an instance of CPSolver
 * - allTasks : an array containing the tasks which could be executed on the machine r.
 * - limit    : the limit of resource (capacity) available on the machine r.
 * - r        : the id of the machine assigned to this constraint.
 * 
 * @date: 22/07/2012
 * @authors: Renaud Hartert - ren.hartert@gmail.com
 *
 * @references:
 * - [1] A New Multi-Resource cumulatives Constraint with Negative Heights,
 *       Nicolas Beldiceanu and Mats Carlsson   
 * - [2] Choco's class CumulSweep.java
 */

class MaxCumulative (cp: CPSolver, allTasks : Array[CumulativeActivity], limit : Int, r : Int) extends Constraint(cp, "MaxCumulative") {

	// Keeps only the relevant tasks
	// TODO: add a reversible set computing task at each iteration
	val tasks = allTasks.filter(_.machine.hasValue(r))
	
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
        		//TODO: check machine on value in order to compute the set of tasks
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
	
	def nextEvent() = if (eventPointSeries.size > 0) eventPointSeries.dequeue else null
	
	/**
	 * As profile events are mandatory to allow pruning, the algorithm returns false if 
	 * no profile event has been generated
	 * 
	 * Note: the max cumulative constraint does not require Check events.
	 */
	def generateEventPointSeries() : Boolean = {
		
		// True if a profile event has been generated
		var profileEvent = false
		
		// Reset eventPointSeries
		eventPointSeries.clear
		
		for (i <- Tasks) {
			
			if (tasks(i).lst < tasks(i).ect && tasks(i).machine.isBoundTo(r)) {
				
				// Profile (Bad : on compulsory part)
				if (tasks(i).minResource > 0) {
					
					// Generates events
					eventPointSeries enqueue eventList(i).getSBadProfile
					eventPointSeries enqueue eventList(i).getEBadProfile

					profileEvent = true
				}			
			}
			
			if (tasks(i).machine.hasValue(r)) {
				
				// Profile (Good : on entire domain)
				if (tasks(i).minResource < 0) {
					
					// Generates events		
					eventPointSeries enqueue eventList(i).getSGoodProfile
					eventPointSeries enqueue eventList(i).getEGoodProfile
					
					profileEvent = true
				}
				
				// Pruning (if something is not fixed)
				if (!(tasks(i).start.isBound && tasks(i).end.isBound && tasks(i).machine.isBoundTo(r) && tasks(i).resource.isBound)) {
					
					// Generates event
					eventPointSeries enqueue eventList(i).getPruning
				}
			}			
		}
		
		return profileEvent	
	}
	
	def resetSweepLine() = {
			
		delta = 0
		sumHeight = 0
		stackPrune.clear
			
		for (i <- Tasks) contribution(i) = 0
	}

	def sweepAlgorithm() : CPOutcome = {
		
		// Reset the parameters of the sweep line
		resetSweepLine()
		
		var event = nextEvent()
		var delta = event.date
		
		while(event != null) {
		
			if (!event.isPruningEvent) {
				
				// If we have considered all the events of the previous date
				if (delta != event.date) {
					
					// Consistency check
					if (sumHeight > limit) 
						return CPOutcome.Failure
					
					// Pruning
					if (prune(r, delta, event.date - 1) == CPOutcome.Failure) 
						return CPOutcome.Failure
						
					// New date to consider
					delta = event.date	
				}
				if (event.isProfileEvent) {
					
					// Adjust resource consumption
					sumHeight += event.increment
					contribution(event.task) += event.increment
				}
			}
			else {
				stackPrune.add(event.task)
			}
			
			event = nextEvent()
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
			
			if (tasks(t).lct <= up + 1) {
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
	
	def pruneForbiden(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (sumHeight - contribution(t) + tasks(t).minResource > limit) {
			
			if (tasks(t).ect > low && tasks(t).lst <= up && tasks(t).minDuration > 0) {
					
				if (tasks(t).machine.removeValue(r) == CPOutcome.Failure) 
					return CPOutcome.Failure
				
			} else if (tasks(t).machine.isBoundTo(r)) {
				
				if (tasks(t).minDuration > 0) {
					
					// This pruning can cause hole in the domains
					for (i <- low - tasks(t).minDuration+1 to up) {
						
						if (tasks(t).start.removeValue(i) == CPOutcome.Failure) 
							return CPOutcome.Failure
					}
					
					// This pruning can cause hole in the domains
					for (i <- low + 1 to up + tasks(t).minDuration) {
						
						if (tasks(t).end.removeValue(i) == CPOutcome.Failure) 
							return CPOutcome.Failure
					}
				}
				
				// This pruning is not necessary if the duration is fixed
				if (!tasks(t).dur.isBound) {
					// Adjust the maximal duration of the task
					val maxD = max(max(low - tasks(t).est, tasks(t).lct -up - 1), 0)
					
					if (tasks(t).dur.updateMax(maxD) == CPOutcome.Failure) 
						return CPOutcome.Failure
				}
			}
		}
			
		return CPOutcome.Suspend
	}
	
	def pruneConsumption(t : Int, r : Int, low : Int, up : Int) : CPOutcome = {
		
		if (tasks(t).machine.isBoundTo(r) && tasks(t).ect > low && tasks(t).lst <= up && tasks(t).minDuration > 0) {
			
			if (tasks(t).resource.updateMax(limit - (sumHeight - contribution(t))) == CPOutcome.Failure) 
				return CPOutcome.Failure
		}
			
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
	
	// The event structure used to increase the speed of the algorithm by preprocessing.
	class EventList(t : Int) {
		
		val sCheck       : Event = new Event(EventType.Check, t, 0, 1)
		val eCheck       : Event = new Event(EventType.Check, t, 0, -1)
		val sBadProfile  : Event = new Event(EventType.Profile, t, 0, 0)
		val eBadProfile  : Event = new Event(EventType.Profile, t, 0, 0)
		val sGoodProfile : Event = new Event(EventType.Profile, t, 0, 0)
		val eGoodProfile : Event = new Event(EventType.Profile, t, 0, 0)
		val Pruning      : Event = new Event(EventType.Pruning, t, 0, 0)
		
		def getSCheck : Event = {
			sCheck.date = tasks(sCheck.task).lst
			return sCheck
		}
		
		def getECheck : Event = {
			eCheck.date = tasks(eCheck.task).ect
			return eCheck
		}
		
		def getSBadProfile : Event = {
			sBadProfile.date = tasks(sBadProfile.task).lst
			sBadProfile.increment = tasks(sBadProfile.task).minResource
			return sBadProfile
		}
		
		def getEBadProfile : Event = {
			eBadProfile.date = tasks(eBadProfile.task).ect
			eBadProfile.increment = -tasks(eBadProfile.task).minResource
			return eBadProfile
		}
		
		def getSGoodProfile : Event = {
			sGoodProfile.date = tasks(sGoodProfile.task).est
			sGoodProfile.increment = tasks(sGoodProfile.task).minResource
			return sGoodProfile
		}
		
		def getEGoodProfile : Event = {
			eGoodProfile.date = tasks(eGoodProfile.task).lct
			eGoodProfile.increment = -tasks(eGoodProfile.task).minResource
			return eGoodProfile
		}
		
		def getPruning : Event = {
			Pruning.date = tasks(Pruning.task).est
			return Pruning
		}
	}
}