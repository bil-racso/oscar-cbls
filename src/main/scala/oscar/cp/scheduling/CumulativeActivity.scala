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

import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;

class CumulativeActivity(activity: Activity, resourceVar : CPVarInt, heightVar : CPVarInt) {

    def resource = resourceVar
    def height   = heightVar
    
    def start = activity.start
    def end   = activity.end
    def dur   = activity.dur
    
    def est   = activity.est
    def lst   = activity.lst
    def ect   = activity.ect
    def lct   = activity.lct
    
    def minDuration = activity.minDuration
    def maxDuration = activity.maxDuration
  
	/**
	 * smallest quantity of resource
	 */
	def minResource() = resource.min
	
	/**
	 * largest quantity of resource
	 */
	def maxResource() = resource.max
	
	/**
	 * smallest quantity of resource
	 */
	def minHeight = height.min
	
	/**
	 * largest quantity of resource
	 */
	def maxHeight = height.max
	
	override def toString() = activity + " height: " + heightVar
}

object CumulativeActivity {
	
	def apply(activity : Activity, resourceVar : Int, heightVar : Int) = {
		
		new CumulativeActivity(activity, CPVarInt(activity.store, resourceVar), CPVarInt(activity.store, heightVar))
	}
	
	def apply(activity : Activity, resourceVar : CPVarInt, heightVar : Int) = {
		
		new CumulativeActivity(activity, resourceVar, CPVarInt(activity.store, heightVar))
	}
	
	def apply(activity : Activity, resourceVar : Int, heightVar : CPVarInt) = {
		
		new CumulativeActivity(activity, CPVarInt(activity.store, resourceVar), heightVar)
	}
	
	def apply(activity : Activity, resourceVar : CPVarInt, heightVar : CPVarInt) = {
		
		new CumulativeActivity(activity, resourceVar, heightVar)
	}
}