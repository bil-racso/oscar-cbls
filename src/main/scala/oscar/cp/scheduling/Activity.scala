/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.scheduling;

import java.security.InvalidParameterException

import oscar.cp.core.CPVarInt
import oscar.cp.core.Store
import oscar.cp.constraints.LeEq
import oscar.cp.modeling.CPScheduler
import oscar.cp.core.CPOutcome

class Activity(val scheduler : CPScheduler, startVar : CPVarInt, durVar : CPVarInt, endVar : CPVarInt, n : String = null, existingId : Option[Int] = None) {

	// Linking the variables
	scheduler.add(startVar + durVar == endVar)
	// If no id is specified, link the activity to the scheduler and get an id
	val id = existingId.getOrElse(scheduler.addActivity(this))
	// Default name
	val name = Option(n) getOrElse ("Activity " + id)

	// The variables
	def start = startVar
	def end   = endVar
	def dur   = durVar

	// Earliest starting time
	def est = start.min
	// Latest starting time
	def lst = start.max
	// Earliest completion time assuming the smallest duration
	def ect = end.min
	// Latest completion time assuming the smallest duration
	def lct = end.max

	// Current minimal duration of this activity
	def minDuration = dur.min
	// Current maximal duration of this activity
	def maxDuration = dur.max

	def store = scheduler

	override def toString = name + "(s: " + start + ", d: " + dur + ", e: " + end + ")"

	def adjustStart(v : Int) = start.updateMin(v)

	// Precedences 
	// -----------------------------------------------------------

	def precedes(act : Activity) = endsBeforeStartOf(act)

	def endsBeforeEndOf(act : Activity) = ActivityPrecedence(this, act, EBE)
	def endsBeforeStartOf(act : Activity) = ActivityPrecedence(this, act, EBS)
	def startsBeforeEndOf(act : Activity) = ActivityPrecedence(this, act, SBE)
	def startsBeforeStartOf(act : Activity) = ActivityPrecedence(this, act, SBS)

	def endsAtEndOf(act : Activity) = ActivityPrecedence(this, act, EAE)
	def endsAtStartOf(act : Activity) = ActivityPrecedence(this, act, EAS)
	def startsAtEndOf(act : Activity) = ActivityPrecedence(this, act, SAE)
	def startsAtStartOf(act : Activity) = ActivityPrecedence(this, act, SAS)
	
	// Start and End
	// -----------------------------------------------------------
	
	def startsEarlierAt(t : Int) = { scheduler.add(startVar >= t) }
	def startsAt(t : Int)        = { scheduler.add(startVar == t) }
	def endsLaterAt(t : Int)     = { scheduler.add(endVar <= t) }
	def endsAt(t : Int)          = { scheduler.add(endVar == t) }

	// Needs / Gives
	// -----------------------------------------------------------

	// UnitResource
	def needs(resource : UnitResource) {
		resource.addActivity(this)
	}

	def needsF(resource : CumulativeResource, capacity : ImplicitVarInt, atEnd : Boolean = true) {
		if (!capacity.isPositive) throw new InvalidParameterException("the capacity must be positive")
		resource.addProdConsActivity(this, capacity.variable(scheduler), atEnd)
	}

	def givesF(resource : CumulativeResource, capacity : ImplicitVarInt, atEnd : Boolean = true) {
		if (!capacity.isPositive) throw new InvalidParameterException("the capacity must be positive")
		resource.addProdConsActivity(this, capacity.opposite(scheduler), atEnd)
	}

	// CumulativeResourceSet	
	def needs(height : ImplicitVarInt) = {
		new NeedsUsage(this, height)
	}

	def gives(height : ImplicitVarInt) = {
		new GivesUsage(this, height)
	}
	
	/**
	 * forces the update of start, end, dur
	 */
	def update() : CPOutcome = {
      // end <= start
      if (end.updateMin(start.min) == CPOutcome.Failure) {
        CPOutcome.Failure
      }
      else if (start.updateMax(end.max) == CPOutcome.Failure) {
        CPOutcome.Failure
      }
      // end = start + dur
      else if (end.updateMax(start.max+dur.max) == CPOutcome.Failure) {
        CPOutcome.Failure
      }
      else if (end.updateMin(start.min+dur.min) == CPOutcome.Failure) {
        CPOutcome.Failure
      }
      // start = end - dur
      else if (start.updateMax(end.max-dur.min) == CPOutcome.Failure) {
        CPOutcome.Failure
      }
      else if (start.updateMin(end.min-dur.max) == CPOutcome.Failure) {
        CPOutcome.Failure
      }
      // dur = end - start
      else if (dur.updateMax(end.max-start.min) == CPOutcome.Failure) {
        CPOutcome.Failure
      }
      else if (dur.updateMin(end.min-start.max) == CPOutcome.Failure) {
        CPOutcome.Failure
      }
      else CPOutcome.Suspend
    }
}

object Activity {

	def apply(scheduler : CPScheduler, dur : ImplicitVarInt) = {

		val durVar = dur.variable(scheduler)

		val startVar : CPVarInt = CPVarInt(scheduler, 0 to scheduler.horizon - durVar.min)
		val endVar   : CPVarInt = CPVarInt(scheduler, durVar.min to scheduler.horizon)

		new Activity(scheduler, startVar, durVar, endVar)
	}

	def apply(scheduler : CPScheduler, dur : ImplicitVarInt, name : String) = {

		val durVar = dur.variable(scheduler)

		val startVar : CPVarInt = CPVarInt(scheduler, 0 to scheduler.horizon - durVar.min)
		val endVar   : CPVarInt = CPVarInt(scheduler, durVar.min to scheduler.horizon)

		new Activity(scheduler, startVar, durVar, endVar, n = name)
	}
}

class MirrorActivity(val act : Activity) extends Activity(act.scheduler, act.start, act.dur, act.end, n = act.name, existingId = Option(act.id)) {

	override def start : CPVarInt = throw new UninitializedFieldError("not available")
	override def end : CPVarInt   = throw new UninitializedFieldError("not available")

	// Earliest starting time
	override def est = -act.lct;
	// Latest starting time
	override def lst = -act.ect;
	// Earliest completion time assuming the smallest duration
	override def ect = -act.lst
	// Latest completion time assuming the smallest duration
	override def lct = -act.est

	override def adjustStart(v : Int) = end.updateMax(-v)

	override def toString() = "mirror of activity:" + act;

	// Precedences
	override def precedes(act : Activity) = throw new UninitializedFieldError("not available")
	override def endsBeforeEndOf(act : Activity) = throw new UninitializedFieldError("not available")
	override def endsBeforeStartOf(act : Activity) = throw new UninitializedFieldError("not available")
	override def startsBeforeEndOf(act : Activity) = throw new UninitializedFieldError("not available")
	override def startsBeforeStartOf(act : Activity) = throw new UninitializedFieldError("not available")
	override def endsAtEndOf(act : Activity) = throw new UninitializedFieldError("not available")
	override def endsAtStartOf(act : Activity) = throw new UninitializedFieldError("not available")
	override def startsAtEndOf(act : Activity) = throw new UninitializedFieldError("not available")
	override def startsAtStartOf(act : Activity) = throw new UninitializedFieldError("not available")
}
