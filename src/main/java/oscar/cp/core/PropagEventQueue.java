/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
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
