/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.core;

import java.util.Collection;
import java.util.LinkedList;
import java.util.Random;

import scampi.cp.constraints.CPObjective;
import scampi.cp.constraints.CPObjectiveMaximize;
import scampi.cp.constraints.CPObjectiveMinimize;
import scampi.cp.constraints.Eq;
import scampi.reversible.ReversiblePointer;
import scampi.reversible.ReversibleSearchNode;



/**
 * Constraint Programming Store
 * @author Pierre Schaus pschaus@gmail.com
 */
public class Store extends ReversibleSearchNode {
	
	/**
	 * Number of call to propagate method in any constraints
	 */
	private int nbPropag = 0;

    /**
     * The highest priority for an Level 1 filtering method (the lowest priority is 0)
     */
	public static final int MAXPRIORL1 = 2;
    /**
     * The highest priority for the propagate method i.e. L2 (the lowest priority is 0)
     */
	public static final int MAXPRIORL2 = 7;
	
	private LinkedList<PropagEvent> [] propagQueueL1;
	private LinkedList<Constraint> [] propagQueueL2;
	
	private ReversiblePointer<CPOutcome> status;
	
	private long timeInFixPoint = 0;
	
	private int highestPriorL1;
	private int highestPriorL2;

	
	@SuppressWarnings("unchecked")
	public Store(){
		super();
		
		status = new ReversiblePointer<CPOutcome>(this,CPOutcome.Suspend);
		status.setValue(CPOutcome.Suspend);

		propagQueueL1 = new LinkedList[MAXPRIORL2+1];
		for (int i = 0; i < propagQueueL1.length; i++) {
			propagQueueL1[i] = new LinkedList<PropagEvent>();
		}
		propagQueueL2 = new LinkedList[MAXPRIORL2+1];
		for (int i = 0; i <= MAXPRIORL2; i++) {
			propagQueueL2[i] = new LinkedList<Constraint>();
		}
		highestPriorL2 = 0;
	}
	
	/**
	 * @return the number of call to propagate method in anyone of the constraints
	 */
	public int getNbPropag() {
		return nbPropag;
	}	
	
	private void optimize(CPObjective objective) {
		CPOutcome oc = post(objective.getConstraint());
		assert(oc != CPOutcome.Failure);
		setObjective(objective);
	}

    /**
     * Define the store as a maximization problem of variable obj
     * @param obj the variable to maximize
     */
	public void maximization(CPVarInt obj){
		optimize(new CPObjectiveMaximize(obj));
	}

    /**
     * Define the store as a minimization problem of variable obj
     * @param obj the variable to minimize
     */
	public void minimization(CPVarInt obj){
		optimize(new CPObjectiveMinimize(obj));
	}

    /**
     * @return The total time spent in the fix point algorithm (usually the significant part)
     */
	public long getTimeInFixPoint(){
		return timeInFixPoint;
	}
	
	public CPOutcome getStatus(){
		return status.getValue();
	}
	
	@Override
	public boolean isFailed(){
		return getStatus() == CPOutcome.Failure;
	}

    /**
     * Set the status of the store to a failed state
     */
	@Override
	public void fail() {
		status.setValue(CPOutcome.Failure);
	}
	
	private void cleanQueues(){
		for (int i = 0; i <= MAXPRIORL1; i++) {
			propagQueueL1[i].clear();
		}
		for (int i = 0; i <= MAXPRIORL2; i++) {
			propagQueueL2[i].clear();
		}
	}

    private int addQueueL2(Constraint c) {
         if ((c.isActive() && !c.isInQueue()) && (!c.inPropagate() || !c.idempotent)) {
			c.setInQueue();
			propagQueueL2[c.getPriorityL2()].add(c);
			return c.getPriorityL2();
		} else {
			return 0;
		}
    }

    /**
     * Notify the constraints that is enqueue them in the L2 propagation queue such that their propagate method
     * is called at some point in the current fix point
     * @param constraints
     */
	protected void notifyL2(Queue<Constraint> constraints) {
		Queue<Constraint> q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			int p = addQueueL2(c);
			highestPriorL2 = Math.max(p, highestPriorL2);
			q = q.getNext();
		}
	}
	
	private void addQueueL1(PropagEvent evt) {
		propagQueueL1[evt.getPrior()].add(evt);
		highestPriorL1 = Math.max(highestPriorL1, evt.getPrior());
	}
	
	protected void notifRemoveL1(PropagEventQueue constraints, CPVarInt var, int val) {
		PropagEventQueue q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			int d = q.getDelta();
			if(c.isActive()){
				addQueueL1(new PropagEventRemoveValue(c,q.getVar(),val+d));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyRemoveIdxL1(PropagEventQueue constraints, CPVarInt var, int val) {
		PropagEventQueue q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			int d = q.getDelta();
			int idx = q.getIdx();
			if(c.isActive()){
				addQueueL1(new PropagEventRemoveValueIdx(c,q.getVar(),idx,val+d));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyUpdateMinL1(PropagEventQueue constraints, CPVarInt var, int val) {
		PropagEventQueue q = constraints;
		while(q != null) {
			Constraint c = q.getElem();
			int d = q.getDelta();
			if(c.isActive()){
				addQueueL1(new PropagEventUpdateMin(c,q.getVar(),val+d));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyUpdateMinIdxL1(PropagEventQueue constraints, CPVarInt var, int val) {
		PropagEventQueue q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			int d = q.getDelta();
			int idx = q.getIdx();
			if(c.isActive()){
				addQueueL1(new PropagEventUpdateMinIdx(c,q.getVar(),idx,val+d));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyBindL1(PropagEventQueue constraints, CPVarInt var) {
		PropagEventQueue q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			if(c.isActive()){
				addQueueL1(new PropagEventBindToValue(c,q.getVar()));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyBindIdxL1(PropagEventQueue constraints, CPVarInt var) {
		PropagEventQueue q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			int idx = q.getIdx();
			if (c.isActive()) {
				addQueueL1(new PropagEventBindToValueIdx(c,q.getVar(),idx));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyUpdateMaxL1(PropagEventQueue constraints, CPVarInt var, int val) {
		PropagEventQueue q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			int d = q.getDelta();
			if (c.isActive()) {
				addQueueL1(new PropagEventUpdateMax(c,q.getVar(),val+d));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyUpdateMaxIdxL1(PropagEventQueue constraints, CPVarInt var, int val) {
		PropagEventQueue q = constraints;
		while (q!=null) {
			Constraint c = q.getElem();
			int d = q.getDelta();
			int idx = q.getIdx();
			if (c.isActive()) {
				addQueueL1(new PropagEventUpdateMaxIdx(c,q.getVar(),idx,val+d));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyUpdateBoundsL1(PropagEventQueue constraints, CPVarInt var) {
		PropagEventQueue  q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			if (c.isActive()) {
				addQueueL1(new PropagEventUpdateBounds(c,q.getVar()));
			}
			q = q.getNext();
		}
	}
	
	protected void notifyUpdateBoundsIdxL1(PropagEventQueue constraints, CPVarInt var) {
		PropagEventQueue q = constraints;
		while (q != null) {
			Constraint c = q.getElem();
			int idx = q.getIdx();
			if (c.isActive()) {
				// we must do q.getVar rather than var because it might be a view of the var that did the domain modif
				addQueueL1(new PropagEventUpdateBoundsIdx(c,q.getVar(),idx)); 
			}
			q = q.getNext();
		}
	}

    /**
     * Fix Point algorithm
     * @return Failure is the fix point detects a failure that is one of the domain became empty, Suspend otherwise
     */
	private CPOutcome propagate() {
		assert(status.getValue() != CPOutcome.Failure);
		
		long t0 = System.currentTimeMillis();
		
		CPOutcome ok = !getObjective().isOK() ? CPOutcome.Failure: CPOutcome.Suspend;
		
		while (ok != CPOutcome.Failure) {
			int p;
			
			while (ok != CPOutcome.Failure && !isL1QueueEmpty()) {
				for (int i = MAXPRIORL1; i >= 0; i--) {
					if(!propagQueueL1[i].isEmpty()) {
						PropagEvent event = propagQueueL1[i].removeFirst();
						ok = event.propagate();
						break;
					}
				}
			}
			p = MAXPRIORL2;
			while (p >= 0 && propagQueueL2[p].isEmpty())  --p;
			if (p<0) break;
			while (ok != CPOutcome.Failure && !propagQueueL2[p].isEmpty()) {
				Constraint c = propagQueueL2[p].removeFirst();
				highestPriorL2 = p;
				nbPropag++;
				ok = c.execute();
				if (highestPriorL2 > p || !isL1QueueEmpty()) break;
			}
		}
		timeInFixPoint += System.currentTimeMillis()-t0;
		return ok==CPOutcome.Failure ? ok : CPOutcome.Suspend;
	}
	
	private boolean isL1QueueEmpty() {
		for(int i = 0; i <= MAXPRIORL1; i++) {
			if (!propagQueueL1[i].isEmpty()) return false;
		}
		return true;
	}

    /**
     * Add a constraint to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c, the constraint
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise
     */
	public CPOutcome post(Constraint c) {
		return post(c, CPPropagStrength.Weak);
	}
	
    /**
     * Add a constraint b == true to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c, the constraint
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise
     */
	public CPOutcome post(CPVarBool b) {
		return post(new Eq(b, 1), CPPropagStrength.Weak);
	}	

    /**
     * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c
     * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint).
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
	public CPOutcome post(Constraint c, CPPropagStrength st) {
		if (status.getValue() == CPOutcome.Failure) return CPOutcome.Failure; 
		CPOutcome oc = c.setup(st);
		if (oc != CPOutcome.Failure) {
			if (oc == CPOutcome.Success) {
				c.deactivate();
			}
			//don't forget that posting a constraint can also post other constraints (e.g. reformulation)
			//so we must propagate because the queues may not be empty.
			oc = propagate();
		}
		if (oc == CPOutcome.Failure) {//failure
			cleanQueues();
		}
		status.setValue(oc);
		return status.getValue();
	}

    /**
     * Add a set of constraints to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm afterwards.
     * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
     * @param constraints
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
	public CPOutcome post(Collection<Constraint> constraints) {
		return post(constraints,CPPropagStrength.Weak);
	}

    /**
     * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
     * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
     * @param constraints
     * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
	public CPOutcome post(Collection<Constraint> constraints, CPPropagStrength st) {
		if (status.getValue() == CPOutcome.Failure) return CPOutcome.Failure;
		CPOutcome oc = CPOutcome.Suspend;
		for (Constraint c : constraints) {
			oc = c.setup(st);
			if (oc == CPOutcome.Success) {
				c.deactivate();
			}
			status.setValue(oc);
			if (oc == CPOutcome.Failure) {
				cleanQueues();
				return oc;
			}
		}
		oc = propagate();
		if (oc == CPOutcome.Failure) {
			cleanQueues();
		}
		status.setValue(oc);
		return status.getValue();
	}

    /**
     * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c
     * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
     */
    public void add(Constraint c) {
        if (post(c) == CPOutcome.Failure || getStatus() == CPOutcome.Failure) {
            throw  new NoSolutionException("the store failed when adding constraint :"+c);
        }
    }

    /**
     * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c
     * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint).
     * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
     */
    public void add(Constraint c, CPPropagStrength st) {
        if (post(c,st) == CPOutcome.Failure || getStatus() == CPOutcome.Failure) {
            throw  new NoSolutionException("the store failed when adding constraint :"+c);
        }
    }
    
    /**
     * Add a constraint to the store (b == true) in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c
     * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
     */
    public void add(CPVarBool b) {
        if (post(new Eq(b, 1)) == CPOutcome.Failure || getStatus() == CPOutcome.Failure) {
            throw  new NoSolutionException("the store failed when setting boolvar " + b + " to true");
        }
    }

    /**
     * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
     * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
     * @param constraints
     * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
    public void add(Collection<Constraint> constraints) {
        if (post(constraints) == CPOutcome.Failure || getStatus() == CPOutcome.Failure) {
            throw  new NoSolutionException("the store failed when adding constraints :"+constraints);
        }
    }

    /**
     * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
     * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
     * @param constraints
     * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
     * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
    public void add(Collection<Constraint> constraints, CPPropagStrength st) {
        if (post(constraints,st) == CPOutcome.Failure || getStatus() == CPOutcome.Failure) {
            throw  new NoSolutionException("the store failed when adding constraints :"+constraints);
        }
    }

}
