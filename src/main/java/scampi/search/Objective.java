/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.search;

import java.util.LinkedList;



 /**
  * @author Pierre Schaus pschaus@gmail.com
  */
public abstract class Objective {
	
	protected int objVal;
	LinkedList<ObjectiveObserver> observers;
	
	public Objective() {
		observers = new LinkedList<ObjectiveObserver>();
	}
	
	public void notifyNewBound() {
		for (ObjectiveObserver o: observers) {
			o.newObj(getBound());
		}
	}
	
	public abstract void tighten();
	
	abstract public void relax();
	
	public void setNewBound(int val) {
		objVal = val;
		notifyNewBound();
	}

    /**
     * @return the current lower/uppper bound in a maximization/minimization problem
     */
	public int getBound() {
		return objVal;
	}

    /**
     * @return an upper/lower bound on the possible objective in a maximization/minimization problem
     */
	abstract public int getOptimumBound();

    /**
     *
     * @return true if the objective is at optimum, that is if the current bound is better than the optimum bound
     */
    abstract public boolean isOptimum();
    
    /**
     * 
     * @return
     */
    abstract public boolean isOK();

}
