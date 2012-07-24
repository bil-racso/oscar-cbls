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
package oscar.cp.scheduling;


import oscar.cp.core.CPVarInt;
import oscar.cp.core.Store;

class Activity(val start: CPVarInt, val dur: CPVarInt) {
    
    val end = start.plus(dur)
	
	def this(start: CPVarInt,dur: Int) = this(start,new CPVarInt(start.getStore(),dur,dur))

	/**
	 * earliest starting time
	 */
	def est() = start.getMin()
	
	
	/**
	 * latest starting time
	 */
	def lst() = start.getMax()
	
	/**
	 * earliest completion time assuming the smallest duration
	 */
	def ect() = end.getMin()
	
	/**
	 * latest completion time assuming the smallest duration
	 */
	def lct() = end.getMax()
	
	/**
	 * current minimal duration of this activity
	 */
	def minDuration() = dur.getMin()

	/**
	 * current maximal duration of this activity
	 */
	def maxDuration() = dur.getMax();
	
	
	override def toString() = "dur:"+dur+ " in ["+est+","+lct+"[";

}



class MirrorActivity(val act: Activity)  extends Activity() {

	private Activity act;

	public MirrorActivity(Activity act) {
		this.act = act;
	}

	/**
	 * earliest starting time
	 */
	public int getEST() {
		return - act.getLCT();
	}

	/**
	 * latest starting time
	 */
	public int getLST() {
		return - act.getECT();
	}

	/**
	 * earliest completion time assuming the smallest duration
	 */
	public int getECT() {
		return - act.getLST();
	}

	/**
	 * latest completion time assuming the smallest duration
	 */
	public int getLCT() {
		return - act.getEST();
	}

	/**
	 * current minimal duration of this activity
	 */
	public int getMinDuration() { 
		return act.getMinDuration();
	}


	/**
	 * current maximal duration of this activity
	 */
	public int getMaxDuration() { 
		return act.getMaxDuration(); 
	}
}
