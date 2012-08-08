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

package oscar.cp.scheduling

import oscar.cp.core.CPVarInt
import oscar.cp.core.Store
import oscar.cp.modeling.CPScheduler

class CumulativeActivity(scheduler : CPScheduler, startVar : CPVarInt, durVar : CPVarInt, endVar : CPVarInt, resourceVar : CPVarInt, heightVar : CPVarInt) extends Activity(scheduler, startVar, durVar, endVar) {
	
    def resource  = resourceVar
    def height    = heightVar
	def minHeight = height.min
	def maxHeight = height.max
	
	override def toString() = super.toString + " height: " + heightVar + " resource: " + resourceVar
}

object CumulativeActivity {
	
	def apply(scheduler : CPScheduler, start : ImplicitVarInt, dur : ImplicitVarInt, end : ImplicitVarInt, resource : ImplicitVarInt, height : ImplicitVarInt) = {
		
		val startVar    = start.variable(scheduler)
		val durVar      = dur.variable(scheduler)
		val endVar      = end.variable(scheduler)
		val resourceVar = resource.variable(scheduler)
		val heightVar   = height.variable(scheduler)
		
		new CumulativeActivity(scheduler, startVar, durVar, endVar, resourceVar, heightVar)
	}
	
	def apply(activity : Activity, resource : ImplicitVarInt, height : ImplicitVarInt) = {
		
		val scheduler   = activity.scheduler
		val startVar    = activity.start
		val durVar      = activity.dur
		val endVar      = activity.end
		val resourceVar = resource.variable(scheduler)
		val heightVar   = height.variable(scheduler)
		
		new CumulativeActivity(scheduler, startVar, durVar, endVar, resourceVar, heightVar)
	}
}

object ProdConsActivity {

	def apply(activity : Activity, resource : ImplicitVarInt, height : ImplicitVarInt, atEnd : Boolean = true) = {
		
		val scheduler   = activity.scheduler
		val startVar    = if (atEnd) activity.end else activity.start
		val durVar      = CPVarInt(activity.scheduler, 0 to activity.store.horizon)
		val endVar      = CPVarInt(activity.scheduler, activity.scheduler.horizon)
		val resourceVar = resource.variable(scheduler)
		val heightVar   = height.variable(scheduler)
		
		new CumulativeActivity(scheduler, startVar, durVar, endVar, resourceVar, heightVar)
	}
}