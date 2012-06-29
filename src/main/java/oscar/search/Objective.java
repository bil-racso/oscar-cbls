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

package oscar.search;

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
