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
public class ReversibleOrderedSet extends AbstractOrderedSet {
	
	private ReversibleInt size;
	private ReversibleInt first;
	private ReversibleInt last;
	private ReversibleInt [] prev;
	private ReversibleInt [] next;
	private ReversibleSearchNode n;
	
	public ReversibleOrderedSet(ReversibleSearchNode n, int min, int max) {
		super(min,max);
		this.n = n;
		prev = new ReversibleInt [max-min+1];
		next = new ReversibleInt [max-min+1];
		for (int i = 0; i < max-min+1; i++) {
			prev[i] = new ReversibleInt(n);
			next[i] = new ReversibleInt(n);
		}
		size = new ReversibleInt(n);
		first = new ReversibleInt(n);
		last = new ReversibleInt(n);
		init();
	}
	


	@Override
	protected void setSize(int size) {
		this.size.setValue(size);
	}

	@Override
	public int getSize() {
		return this.size.getValue();
	}

	@Override
	protected void setFirst(int f) {
		this.first.setValue(f);
	}

	@Override
	protected int getFirst() {
		return first.getValue();
	}

	@Override
	protected void setLast(int l) {
		this.last.setValue(l);
	}

	@Override
	protected int getLast() {
		return last.getValue();
	}
	
	@Override
	protected void setNext(int i, int v) {
		this.next[i].setValue(v);
	}

	@Override
	protected int getNext(int i) {
		return next[i].getValue();
	}

	@Override
	protected void setPrev(int i, int v) {
		prev[i].setValue(v);
	}

	@Override
	protected int getPrev(int i) {
		return prev[i].getValue();
	}


}
