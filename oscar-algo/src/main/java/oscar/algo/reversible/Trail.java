/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.algo.reversible;

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
	
	public long getTimeInRestore() {
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

