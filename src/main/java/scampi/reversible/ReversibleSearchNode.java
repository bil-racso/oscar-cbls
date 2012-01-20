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

import java.util.Random;
import java.util.Stack;


/**
 * Class representing a reversible search node, that is a node able to restore all
 * the reversible state attached to it (see Reversibles). <br>
 * A reversible search node is used to find solution by exploration of search tree (see Search).
 * @author Pierre Schaus pschaus@gmail.com
 */
public class ReversibleSearchNode {
	
	
	private int magic;	
	private Trail trail;
	private Stack<TrailEntry> pointerStack = new Stack<TrailEntry>();
	
	protected Objective objective;
	
	public final Random random = new Random(0);
	
	public ReversibleSearchNode() {
		trail = new Trail();
		magic = 0;
		objective = new DummyObjective();
	}
	
	/**
	 * 
	 * @return The Random of this node potentially used in algorithms
	 */
	public Random getRandom() {
		return random;
	}

    /**
     *
     * @return  true if this node can surely not lead to any solution
     */
	public boolean isFailed() {
		return false;
	}

    /**
     *
     * @return an objective if any, null otherwise
     */
	public Objective getObjective() {
		return objective;
	}

    /**
     * Set the objective to optimize
     * @param obj
     */
	protected void setObjective(Objective obj) {
		this.objective = obj;
	}
	
	protected int getMagic() {
		return magic;
	}
	
	public Trail getTrail() {
		return trail;
	}

    /**
     * Store the current state of the node on a stack.
     */
	public void pushState() {
		magic++;
		pointerStack.push(trail.getTopEntry());
	}

    /**
     * Restore state on top of the stack of states and remove it from the stack.
     */
	public void pop() {
		trail.restoreUntil(pointerStack.pop());
		magic++; // increment the magic because we want to trail again
	}

    /**
     * Restore the node to its initial state
     */
	public void popAll() {
		while (!pointerStack.empty()) {
			trail.restoreUntil(pointerStack.pop());
		}
		magic++; // increment the magic because we want to trail again
	}
	
	@Override
	public String toString() {
		return "ReversibleSearchNode: nbPushed"+pointerStack.size()+" currentTrailSize:"+trail.getSize();
	}

}
