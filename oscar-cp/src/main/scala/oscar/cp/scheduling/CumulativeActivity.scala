/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.scheduling

import oscar.cp.core.CPVarInt
import oscar.cp.core.CPStore
import oscar.cp.modeling.CPScheduler

class CumulativeActivity(scheduler : CPScheduler, startVar : CPVarInt, durVar : CPVarInt, endVar : CPVarInt, resourceVar : CPVarInt, heightVar : CPVarInt, n : String = null, existingId: Option[Int] = None) extends Activity(scheduler, startVar, durVar, endVar, n = n, existingId = existingId) {
	
    def resource  = resourceVar
    def height    = heightVar
	def minHeight = height.min
	def maxHeight = height.max
	def minEnergy = minHeight*minDuration
	def maxEnergy = maxHeight*maxDuration
	
	override def toString = name + "(s: " +start+ ", d: " +dur+ ", e: " +end+ ", r: " +resource+ ", h: " +height+ ")"
}

object CumulativeActivity {
	
	def apply(scheduler : CPScheduler, dur : ImplicitVarInt, resource : ImplicitVarInt, height : ImplicitVarInt) = {
		
		val durVar      = dur.toCPVarInt(scheduler)
		val startVar    = CPVarInt(scheduler, 0 to scheduler.horizon - durVar.min)
		val endVar      = CPVarInt(scheduler, durVar.min to scheduler.horizon) 
		val resourceVar = resource.toCPVarInt(scheduler)
		val heightVar   = height.toCPVarInt(scheduler)
		
		new CumulativeActivity(scheduler, startVar, durVar, endVar, resourceVar, heightVar)
	}
	
	def apply(scheduler : CPScheduler, dur : ImplicitVarInt, resource : ImplicitVarInt, height : ImplicitVarInt, name : String) = {
		
		val durVar      = dur.toCPVarInt(scheduler)
		val startVar    = CPVarInt(scheduler, 0 to scheduler.horizon - durVar.min)
		val endVar      = CPVarInt(scheduler, durVar.min to scheduler.horizon) 
		val resourceVar = resource.toCPVarInt(scheduler)
		val heightVar   = height.toCPVarInt(scheduler)
		
		new CumulativeActivity(scheduler, startVar, durVar, endVar, resourceVar, heightVar, n = name)
	}
	
	def apply(activity : Activity, resource : ImplicitVarInt, height : ImplicitVarInt) = {
		
		val scheduler   = activity.scheduler
		val startVar    = activity.start
		val durVar      = activity.dur
		val endVar      = activity.end
		val resourceVar = resource.toCPVarInt(scheduler)
		val heightVar   = height.toCPVarInt(scheduler)
		
		new CumulativeActivity(scheduler, startVar, durVar, endVar, resourceVar, heightVar, n = activity.name, existingId = Option(activity.id))
	}
}

object ProdConsActivity {

	def apply(activity : Activity, resource : ImplicitVarInt, height : ImplicitVarInt, atEnd : Boolean = true) = {
		
		val scheduler   = activity.scheduler
		val startVar    = if (atEnd) activity.end else activity.start
		val durVar      = CPVarInt(activity.scheduler, 0 to activity.store.horizon)
		val endVar      = CPVarInt(activity.scheduler, activity.scheduler.horizon)
		val resourceVar = resource.toCPVarInt(scheduler)
		val heightVar   = height.toCPVarInt(scheduler)
		
		new CumulativeActivity(scheduler, startVar, durVar, endVar, resourceVar, heightVar, n = activity.name, existingId = Option(activity.id))
	}
}

class MirrorCumulativeActivity(val act : CumulativeActivity) extends CumulativeActivity(act.scheduler, act.start, act.dur, act.end, act.resource, act.height, n = act.name, existingId = Option(act.id)) {

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

	override def adjustStart(v : Int) = act.end.updateMax(-v)

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
