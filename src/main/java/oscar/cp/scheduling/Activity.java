/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
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
