/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.reversible;

/**
 * Implementation of an Ordered set using double linked list (prev and succ array) on which you can only remove value (in constant time). 
 * @author Pierre Schaus pschaus@gmail.com
 */
public class OrderedSet extends AbstractOrderedSet {
	

	private int size;
	private int first; // first index in next != -1
	private int last;
	private int [] prev;
	private int [] next;

	public OrderedSet(int min, int max) {
		super(min,max);
		prev = new int [max-min+1];
		next = new int [max-min+1];
		init();
	}
	

	@Override
	protected void setSize(int size) {
		this.size = size;
	}

	@Override
	public int getSize() {
		return size;
	}

	@Override
	protected void setFirst(int f) {
		this.first = f;
	}

	@Override
	protected int getFirst() {
		return first;
	}

	@Override
	protected void setLast(int l) {
		this.last = l;
	}

	@Override
	protected int getLast() {
		return last;
	}
	
	@Override
	protected void setNext(int i, int v) {
		this.next[i] = v;
	}

	@Override
	protected int getNext(int i) {
		return next[i];
	}

	@Override
	protected void setPrev(int i, int v) {
		prev[i] = v;
	}

	@Override
	protected int getPrev(int i) {
		return prev[i];
	}


}
