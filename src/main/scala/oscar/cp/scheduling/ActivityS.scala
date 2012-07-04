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

class ActivityS (start : CPVarInt, duration : CPVarInt, end : CPVarInt) {

	def getDur() = duration
	def getStart() = start
	def getEnd() = end
	
	/**
	 * earliest starting time
	 */
	def getEST() = start.getMin()
	
	/**
	 * latest starting time
	 */
	def getLST() = start.getMax()
	
	/**
	 * earliest completion time assuming the smallest duration
	 */
	def getECT() = end.getMin()
	
	/**
	 * latest completion time assuming the smallest duration
	 */
	def getLCT() = end.getMax()
	
	/**
	 * current minimal duration of this activity
	 */
	def getMinDuration() = duration.getMin()
	
	/**
	 * current maximal duration of this activity
	 */
	def getMaxDuration() = duration.getMax()
	
	override def toString() = "dur:"+getDur()+ " in ["+getEST()+","+getLCT()+"["
}

object ActivityS {
	
	def apply(start : CPVarInt, duration : CPVarInt) = {
		
		new ActivityS(start, duration, start.plus(duration))
	}
	
	def apply(start : CPVarInt, duration : Int) = {
		
		val dur = new CPVarInt(start.getStore(), duration, duration)
		new ActivityS(start, dur, start.plus(dur))
	}
}