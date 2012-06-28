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
 * Represent an propagation event (AC5 Events).
 * An propagation event records a fine grained modification to the domain of 
 * a variable with the associated delta (min changed, val removed...)
 * that must be notified to the constraint.
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class PropagEvent {
	protected Constraint cstr;
	
	public int prior = 0;
	
	/**
	 * Creates an AC5Event for a constraint
	 * @param cstr
	 */
	public PropagEvent(Constraint cstr) {
		this.cstr = cstr;
	}
	
	/**
	 * @return a value between 0 (lowest priority) and Store.MAXPRIORAC5 (highest priority) depending 
	 *         on the priority of the modification. 
	 *         AC5 events with higher priority will always be executed first in the propagation queue
	 */
	
	int getPrior() {	
		return prior;
	}
	
	/**
	 * Notifies the constraint of the change associated the event and propagate the associated AC5 method
	 * @return The outcome of the propagation
	 */
	public abstract CPOutcome notifyConstraint();
	

	public CPOutcome propagate() {
		if(cstr.isActive()){
			CPOutcome oc = notifyConstraint();
			if(oc == CPOutcome.Success){
				cstr.deactivate();
			}
			return oc;
		}
		else return CPOutcome.Suspend;
	}

}
