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
 * Recursive linked queue. <br>
 * A queue contains an element and a pointer to a next queue. <br>
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Queue<T>{
	protected T elem;
	protected Queue<T> next = null;

    /**
     * Creates a new queue with one element inside <br>
     *
     * @param next queue. <br>
     *        Set it to null to create an initial queue with just one element. <br>
     *        To add an element in front of an existing queue q, you have to do: q = new Queue(q,elem).
     * @param elem
     */
	public Queue(Queue<T> next,T elem) {
		this.elem = elem;
		this.next = next;
	}

    /**
     *
     * @return  true if there is an element after the one in this queue
     */
	public boolean hasNext() {
		return this.next != null;
	}

    /**
     * @return the next queue, or null if no next queue (last one).
     */
	public Queue<T> getNext() {
		return next;
	}

    /**
     *
     * @return the element in the current position of the queue
     */
	T getElem() {
		return elem;
	}
	
	@Override
	public String toString() {
		String res = "";
		Queue<T> q = this;
		do {	
			T e = q.getElem();
			res += e.toString() + (q.hasNext() ? "->" : "");
			q = q.getNext();
		} while (q != null);
		return res;
	}
	
	/**
	 * @return The size of the queue in Theta(n)
	 */
	public int getSize() {
		int size = 0;
		Queue<T> q = this;
		do {
			size++;
			q = q.getNext();
		} while(q != null);
		return size;
	}
}
