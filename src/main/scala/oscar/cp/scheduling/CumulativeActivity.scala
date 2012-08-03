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

class CumulativeActivity(activity: Activity, resourceVar : CPVarInt, heightVar : CPVarInt) {//extends Activity(activity.scheduler, activity.dur) {

    def resource = resourceVar
    def height   = heightVar
    
    def start = activity.start
    def end = activity.end
    def dur = activity.dur
    
	def est = activity.est
	def lst = activity.lst
	def ect = activity.ect
	def lct = activity.lct
	
	def minDuration = activity.minDuration
	def maxDuration = activity.maxDuration
	
	def minHeight = height.min
	def maxHeight = height.max
	
	def store = activity.store
	
	override def toString() = activity + " height: " + heightVar + " resource: " + resourceVar
}

object CumulativeActivity {
	
	def apply(activity : Activity, resource : Int, height : Int) = {
		
		new CumulativeActivity(activity, CPVarInt(activity.scheduler, resource), CPVarInt(activity.scheduler, height))
	}
	
	def apply(activity : Activity, resourceVar : CPVarInt, height : Int) = {
		
		new CumulativeActivity(activity, resourceVar, CPVarInt(activity.scheduler, height))
	}
	
	def apply(activity : Activity, resourceVar : CPVarInt, heights : Range) = {
		
		new CumulativeActivity(activity, resourceVar, CPVarInt(activity.scheduler, heights))
	}
	
	def apply(activity : Activity, resource : Int, heights : Range) = {
		
		new CumulativeActivity(activity, CPVarInt(activity.scheduler, resource), CPVarInt(activity.scheduler, heights))
	}
	
	def apply(activity : Activity, resources : Range, heights : Range) = {
		
		new CumulativeActivity(activity, CPVarInt(activity.scheduler, resources), CPVarInt(activity.scheduler, heights))
	}
		
	def apply(activity : Activity, resources : Range, height : Int) = {
		
		new CumulativeActivity(activity, CPVarInt(activity.scheduler, resources), CPVarInt(activity.scheduler, height))
	}
	
	def apply(activity : Activity, resource : Int, heightVar : CPVarInt) = {
		
		new CumulativeActivity(activity, CPVarInt(activity.scheduler, resource), heightVar)
	}
	
	def apply(activity : Activity, resourceVar : CPVarInt, heightVar : CPVarInt) = {
		
		new CumulativeActivity(activity, resourceVar, heightVar)
	}
}