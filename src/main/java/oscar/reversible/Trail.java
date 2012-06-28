/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.reversible;

import java.util.Stack;


/**
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Trail {
	
	private Stack<TrailEntry> stack = new Stack<TrailEntry>();
	
	private long timeInRestore = 0;
	private int maxSize = 0;
	
	public Trail(){
		stack.add(null);
	}
	
	protected TrailEntry getTopEntry(){
		return stack.peek();
	}

	protected void addEntry(Reversible trailable,Object o){
		stack.push(new TrailEntry(trailable,o));
		maxSize = Math.max(maxSize, stack.size());
	}
	
	public long getTimeInRestore(){
		return timeInRestore;
	}
	
	public int getMaxSize(){
		return maxSize;
	}
	
	protected void restoreUntil(TrailEntry e){
		long t0 = System.currentTimeMillis();
		while(stack.peek()!=e){
			stack.pop().restore();
		}
		timeInRestore += System.currentTimeMillis()-t0;
	}
	
	public int getSize(){
		return stack.size();
	}
}

class TrailEntry{
	
	Reversible trailable;
	Object val;
	
	public TrailEntry(Reversible trailable,Object val) {
		this.trailable = trailable;
		this.val = val;
	}
	
	protected void restore(){
		trailable.restore(val);
	}
}
