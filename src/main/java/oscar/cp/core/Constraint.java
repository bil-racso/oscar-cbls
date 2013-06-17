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
package oscar.cp.core;

import oscar.cp.constraints.Garded;
import oscar.cp.core.CPVarBool;
import oscar.cp.core.Constraint;
import oscar.reversible.ReversibleBool;

/**
 * Abstract class extended by any CP constraints
 * @author Pierre Schaus pschaus@gmail.com
 */
public abstract class Constraint {
	protected CPStore s;	
	
	protected int priorityL2 = 7;//CPStore.MAXPRIORL2;
	
	private ReversibleBool active;
	private ReversibleBool inQueue;
	
	
	protected int priorityBindL1 = CPStore.MAXPRIORL1()-1;
	protected int prioritBoundsL1 = CPStore.MAXPRIORL1()-2;
	protected int priorityRemoveL1= CPStore.MAXPRIORL1()-2;
	protected int priorityRequireL1 = CPStore.MAXPRIORL1()-1;
	protected int priorityExcludeL1 = CPStore.MAXPRIORL1()-2;
	
	protected String name="cons";
	
	/**
	 * Set to true when it is currently exexuting the propagate method
	 */
	private boolean executingPropagate = false;
	
	
	/**
	 * True if the constraint is idempotent i.e. calling two times propagate is useless if no other changes occurred
	 * sigma(store) = sigma(sigma(store))
	 */
	protected boolean idempotent = false;
	
	public Constraint(CPStore s){
		this.s = s;
		active = new ReversibleBool(s);
		active.setValue(true);
		inQueue = new ReversibleBool(s);
		inQueue.setValue(false);
	}
	
	public Constraint(CPStore s,String name){
		this(s);
		this.name = name;
	}
	
	/**
	 * @return true if it is currently executing the propagate method.
	 */
	protected boolean inPropagate() {
		return executingPropagate;
	}
	
	/**
	 * Tells the store that this constraint is idempotent which means that if a changes occur during propagate method,
	 * it will not be called again because you know it would be useless.
	 */
	public void setIdempotent() {
		this.idempotent = true;
	}
	
	public boolean isIdempotent() {
		return this.isIdempotent();
	}
	
	/**
	 * Tells the store that this constraint is idempotent or not, which means that if a changes occur during propagate method,
	 * it will not be called again because you know it would be useless
	 */
	public void setIdempotent(boolean idempotent) {
		this.idempotent = idempotent;
	}	
	
	/**
	 * @param b
	 * @return a garded version of this constraint i.e. that will only be posted when b is true
	 */
	public Constraint when(CPVarBool b) {
		return new Garded(b,this,true);
	}
	
	/**
	 * @param b
	 * @return a garded version of this constraint i.e. that will only be posted when b is false
	 */
	public Constraint whenNot(CPVarBool b) {
		return new Garded(b,this,false);
	}
	
	@Override
	public String toString() {
		return "constraint:"+name;
	}
	
	public String getName() {
		return name;
	}
	
	/**
	 * setup the constraint, typically this is the place where 
	 * - the constraint registers to modifications of the domains of variables in its scope
	 * - a first consistency check and propagation is done
	 * @param l
	 * @return The outcome of the first propagation and consistency check
	 */
	protected abstract CPOutcome setup(CPPropagStrength l);

	/**
	 * Get the level 2 priority
	 * @return
	 */
    protected  int getPriorityL2() {
        return priorityL2;
    }

	/**
	 * 
	 * @param set the L2 priority (propagate method) in the propagation queue, a number between 0 and CPStore.MAXPRIORL2 
	 */
	protected void setPriorityL2(int priority) {
		this.priorityL2 = priority;
	}
	
	private int checkL1Prior(int priority) {
		return Math.max(0,Math.min(priority,CPStore.MAXPRIORL1()));
	}
	
	protected void setPriorityBindL1(int priority) {
		this.priorityBindL1 = checkL1Prior(priority);
	}
	
	protected void setPriorityRemoveL1(int priority) {
		this.priorityRemoveL1 = checkL1Prior(priority);
	}
	
	protected void setPriorityBoundsL1(int priority) {
		this.prioritBoundsL1 = checkL1Prior(priority);
	}
	
	protected int getPriorityBindL1() {
		return this.priorityBindL1;
	}
	
	protected int getPriorityRemoveL1() {
		return this.priorityRemoveL1;
	}
	
	protected int getPriorityBoundsL1() {
		return this.prioritBoundsL1;
	}	
	
	protected int getPriorityRequireL1() {
		return this.priorityRequireL1;
	}
	
	protected int getPriorityExcludeL1() {
		return this.priorityExcludeL1;
	}	
	
	
	/**
	 * @return true if the constraint is still active
	 */
	protected boolean isActive() {
		return active.getValue();
	}
	
	/**
	 * @return true if the constraint is still in the propagation queue, false otherwise
	 */
	protected boolean isInQueue() {
		return inQueue.getValue();
	}
	
	/**
	 * Disable the constraint such that it is not propagated any more (will not enter into the propagation queue).
	 * Note that this state is reversible (trailable).
	 */
	protected void deactivate() {
		active.setValue(false);
	}
	
	/**
	 * Reactivate the constraint
	 */
	protected void activate() {
		active.setValue(true);
	}
	
	/**
	 * Propagation method of Level L2 that is called if variable x has asked to do so with
	 * any one of these methods: <br>
	 * - callPropagateWhenMaxChanges <br>
	 * - callPropagateWhenMinChanges <br>
	 * - callPropagateWhenDomainChanges <br>
	 * - callPropagateWhenBind <br>
	 * The (variable,domain) change that has triggered the call to propagate depends of course
	 * on which of the method(s) above was used 
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome propagate() {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callUpdateBoundsIdxWhenBoundsChange(this,idx)
	 * @param x has a new minimum and/or maximum value in its domain since last call
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome updateBounds(CPVarInt x) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callUpdateBoundsIdxWhenBoundsChange(this,idx)
	 * @param x has a new minimum and/or maximum value in its domain since last call
	 * @param idx is a key value that was given to callUpdateMaxIdxWhenMaxChanges(x,this,idx) attached to variable x. 
	 *        This is typically used to retrieve the index of x in an array of variables in constant time
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome updateBoundsIdx(CPVarInt x, int idx) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked so
	 * with the method call x.callUpdateMaxWhenMaxChanges(this)
	 * @param val is the old maximum of x that has changed
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome updateMax(CPVarInt x, int val) {
		return CPOutcome.Suspend;
	}
	
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callUpdateMinIdxWhenMinChanges(this,idx)
	 * @param val is the old maximum of x that has changed 
	 * @param idx is a key value that was given to callUpdateMaxIdxWhenMaxChanges(x,this,idx) attached to variable x. 
	 *        This is typically used to retrieve the index of x in an array of variables in constant time
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome updateMaxIdx(CPVarInt x, int idx, int val) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked so
	 * with the method call x.callUpdateMinWhenMinChanges(this)
	 * @param val is the old minimum of x that has changed 
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome updateMin(CPVarInt x, int val) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callUpdateMinIdxWhenMinChanges(this,idx)
	 * @param val is the old minimum of x that has changed 
	 * @param idx is a key value that was given to callUpdateMinIdxWhenMinChanges(x,this,idx) attached to variable x. 
	 *        This is typically used to retrieve the index of x in an array of variables in constant time
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome updateMinIdx(CPVarInt x, int idx, int val) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callValBind(this)
	 * @param x is bind 
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome valBind(CPVarInt x) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callValBindIdx(this,idx)
	 * @param x is bind 
	 * @param idx is a key value that was given to callValBindIdx(x,idx) attached to variable x. 
	 *        This is typically used to retrieve the index of x in an array of variables in constant time 
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome valBindIdx(CPVarInt x, int idx) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callValRemoveWhenValueRemoved(this)
	 * @param val is a value that has been removed from the domain of x since last call
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome valRemove(CPVarInt x, int val) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callValRemoveIdxWhenValueRemoved(this)
	 * @param val is a value that has been removed from the domain of x since last call
	 * @param idx is a key value that was given to callValBind(x,idx) attached to variable x. 
	 *        This is typically used to retrieve the index of x in an array of variables in constant time  
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome valRemoveIdx(CPVarInt x, int idx, int val) {
		return CPOutcome.Suspend;
	}
	
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callValRequiredWhenValueRequired(this)
	 * @param val is a value that has been put as required in the domain of x since last call
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome valRequired(CPVarSet x, int val) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callValRequiredIdxWhenValueRemovedIdx(this)
	 * @param val is a value that has been put as required in the domain of x since last call
	 * @param idx is a key value that was given to callValRequiredIdxWhenValueRemovedIdx(x,idx) attached to variable x. 
	 *        This is typically used to retrieve the index of x in an array of variables in constant time  
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome valRequiredIdx(CPVarSet x, int idx, int val) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callValExcludeddWhenValueRequired(this)
	 * @param val is a value that has been excluded in the domain of x since last call
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome valExcluded(CPVarSet x, int val) {
		return CPOutcome.Suspend;
	}
	
	/**
	 * Propagation method of Level L1 that is called if variable x has asked to do so
	 * with the method call x.callValExcludedIdxWhenValueRemovedIdx(this)
	 * @param val is a value that has been put as required in the domain of x since last call
	 * @param idx is a key value that was given to callValExcludedIdxWhenValueRemovedIdx(x,idx) attached to variable x. 
	 *        This is typically used to retrieve the index of x in an array of variables in constant time  
	 * @return the outcome i.e. Failure, Success or Suspend
	 */
	protected CPOutcome valExcludedIdx(CPVarSet x, int idx, int val) {
		return CPOutcome.Suspend;
	}	

	
	
	protected CPOutcome execute() {
		inQueue.setValue(false);
		executingPropagate = true;
		CPOutcome oc = propagate();
		executingPropagate = false;
		if (oc == CPOutcome.Success) {
			deactivate();
		}
		return oc;
	}

    protected  void setInQueue() {
        inQueue.setValue(true);
    }

}
