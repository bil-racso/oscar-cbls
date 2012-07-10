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
package oscar.cp.core;

/**
 * Trailable Queue of AC5 events
 * Each entry of the queue stores:
 *  - a delta
 *  - a index
 *  - a variable
 *  @author Pierre Schaus pschaus@gmail.com
 */
public class PropagEventQueue extends Queue<Constraint> {
	
	private int delta;
	private int idx;
	private CPVarInt var;

	protected PropagEventQueue(PropagEventQueue next, Constraint elem, CPVarInt v, int delta) {
		super(next, elem);
		this.var = v;
		this.delta = delta;
		idx = 0;
	}
	
	protected PropagEventQueue(PropagEventQueue next, Constraint elem, CPVarInt v, int idx,int delta) {
		this(next,elem,v,delta);
		this.idx = idx;
	}
	
	protected CPVarInt getVar() {
		return var;
	}
	
	protected int getDelta() {
		return delta;
	}
	
	protected int getIdx() {
		return idx;
	}
	
	public PropagEventQueue getNext() {
		return (PropagEventQueue) next;
	}
	
	@Override
	public String toString() {
		return "PropagEventQueue constraint:"+elem+" var:"+var+" idx:"+idx;
	}
	
	public int getSize() {
		int size = 0;
		PropagEventQueue q = this;
		while (q != null) {
			size++;
			q = q.getNext();
		}
		return size;
	}

}
