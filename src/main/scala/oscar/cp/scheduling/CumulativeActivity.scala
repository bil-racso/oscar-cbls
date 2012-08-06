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

    def resource = resourceVar
    def height   = heightVar
	
	def minHeight = height.min
	def maxHeight = height.max
	
	override def toString() = super.toString + " height: " + heightVar + " resource: " + resourceVar
}

object CumulativeActivity {
	
	def apply(scheduler : CPScheduler, startVar : CPVarInt, durVar : CPVarInt, endVar : CPVarInt, resourceVar : CPVarInt, heightVar : CPVarInt) = new CumulativeActivity(scheduler, startVar, durVar, endVar, resourceVar, heightVar)
	
	def apply(activity : Activity, resource : Int, height : Int)           = build(activity, CPVarInt(activity.scheduler, resource), CPVarInt(activity.scheduler, height)) 	
	def apply(activity : Activity, resource : CPVarInt, height : Int)      = build(activity, resource, CPVarInt(activity.scheduler, height)) 	
	def apply(activity : Activity, resource : Int, height : CPVarInt)      = build(activity, CPVarInt(activity.scheduler, resource), height)	
	def apply(activity : Activity, resource : CPVarInt, height : CPVarInt) = build(activity, resource, height)
	
	def apply(activity : Activity, resource : CPVarInt, height : Range) = build(activity, resource, CPVarInt(activity.scheduler, height)) 	
	def apply(activity : Activity, resource : Int, height : Range)      = build(activity, CPVarInt(activity.scheduler, resource), CPVarInt(activity.scheduler, height)) 	
	def apply(activity : Activity, resource : Range, height : Range)    = build(activity, CPVarInt(activity.scheduler, resource), CPVarInt(activity.scheduler, height)) 	
	def apply(activity : Activity, resource : Range, height : Int)      = build(activity, CPVarInt(activity.scheduler, resource), CPVarInt(activity.scheduler, height)) 	

	private def build(activity : Activity, resource : CPVarInt, height : CPVarInt) = {
		new CumulativeActivity(activity.scheduler, activity.start, activity.dur, activity.end, resource, height)
	}
}

object ProdConsActivity {

	def apply(activity : Activity, resourceVar : Int, heightVar : Int) = build(activity, CPVarInt(activity.scheduler, resourceVar), CPVarInt(activity.scheduler, heightVar), true)
	def apply(activity : Activity, resourceVar : Int, heightVar : Int, atEnd : Boolean) = build(activity, CPVarInt(activity.scheduler, resourceVar), CPVarInt(activity.scheduler, heightVar), atEnd)
	
	private def build(activity : Activity, resourceVar : CPVarInt, heightVar : CPVarInt, atEnd: Boolean) = {
		val startVar = if (atEnd) activity.end else activity.start
		val endVar   = CPVarInt(activity.scheduler, activity.scheduler.horizon)
		val durVar   = CPVarInt(activity.scheduler, 0 to activity.store.horizon)
		new CumulativeActivity(activity.scheduler, startVar, durVar, endVar, resourceVar, heightVar)
	}
}