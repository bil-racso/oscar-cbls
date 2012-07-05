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

public class Activity {
	
	private Store cp;
	private CPVarInt start;
	private CPVarInt end;
	private CPVarInt dur;

	protected Activity() {
		
	}
	
	public Activity(CPVarInt start, CPVarInt dur)  {
		this.dur = dur;
		this.start = start;
		this.end = start.plus(dur);
	}
	
	public Activity(CPVarInt start, int dur)  {
		this(start,new CPVarInt(start.getStore(),dur,dur));
	}
	
	public CPVarInt getDur() {
		return dur;
	}
	
	public CPVarInt getStart() {
		return start;
	}
	
	public CPVarInt getEnd() {
		return end;
	}
	
	/**
	 * earliest starting time
	 */
	public int getEST() {
		return start.getMin();
	}
	
	/**
	 * latest starting time
	 */
	public int getLST() {
		return start.getMax();
	}
	
	/**
	 * earliest completion time assuming the smallest duration
	 */
	public int getECT() {
		return end.getMin();
	}
	
	/**
	 * latest completion time assuming the smallest duration
	 */
	public int getLCT() {
		return end.getMax();
	}
	

	/**
	 * current minimal duration of this activity
	 */
	public int getMinDuration() { 
		return dur.getMin();
	}
	
	
	/**
	 * current maximal duration of this activity
	 */
	public int getMaxDuration() { 
		return dur.getMax(); 
	}
	
	public String toString() {
		return "dur:"+getDur()+ " in ["+getEST()+","+getLCT()+"[";
	}
	
	

}
