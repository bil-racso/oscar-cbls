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

import java.util.Collection;
import java.util.LinkedList;
import java.util.Random;

import oscar.cp.constraints.CPObjective;
import oscar.cp.constraints.CPObjectiveUnit;
import oscar.cp.constraints.Eq;
import oscar.reversible.ReversiblePointer;
import oscar.reversible.ReversibleSearchNode;
import collection.JavaConversions._

import oscar.cp.core.CPOutcome._

/**
 * Constraint Programming CPStore
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPStore extends ReversibleSearchNode {
  	/**
	 * Number of call to propagate method in any constraints
	 */
	private var nbPropag = 0
	
	val  propagQueueL1 = Array.fill(CPStore.MAXPRIORL1+1)(new LinkedList[PropagEvent]())
	val  propagQueueL2 = Array.fill(CPStore.MAXPRIORL2+1)(new LinkedList[Constraint]())
	
	var  status: ReversiblePointer[CPOutcome] = new ReversiblePointer[CPOutcome](this,Suspend);
	
	/**
	 *The total time spent in the fix point algorithm (usually the significant part)
     */
	var timeInFixPoint: Long = 0
	
	var inPropagate = false
	
	val cutConstraints = new LinkedList[Constraint]()
	
	var throwNoSolExceptions = true
	
	private var highestPriorL1 = 0
	private var highestPriorL2 = 0;
	
	override def fail() { status.setValue(Failure) }
	
	
	/**
	 * deactivate the no solution exception when an add is used and an inconsistent model is detected
	 */
	def deactivateNoSolExceptions() {
		throwNoSolExceptions = false;
	}


	override def isFailed() = status.value == CPOutcome.Failure;
	
	
	
	
	private def cleanQueues() {
	  propagQueueL1.foreach(_.clear())
	  propagQueueL2.foreach(_.clear())
	}
	
	def addQueueL2(c: Constraint): Int = {
        if ((c.isActive() && !c.isInQueue()) && (!c.inPropagate() || !c.idempotent)) {
			c.setInQueue()
			propagQueueL2(c.getPriorityL2()).add(c)
			c.getPriorityL2()
		} else {
			0
		}
    }
	
    /**
     * Notify the constraints that is enqueue them in the L2 propagation queue such that their propagate method
     * is called at some point in the current fix point
     * @param constraints
     */
	def notifyL2(constraints: ConstraintQueue) {
		var q = constraints;
		while (q != null) {
			val c = q.cons;
			val p = addQueueL2(c);
			highestPriorL2 = Math.max(p, highestPriorL2);
			q = q.next
		}
	}
	
	private def addQueueL1(evt:PropagEvent ) {
		propagQueueL1(evt.prior).add(evt);
		highestPriorL1 = Math.max(highestPriorL1, evt.prior);
	}
	
	def notifRemoveL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
		var q = constraints;
		while (q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventRemoveValue(q.cons,q.x,v+q.delta));
			}
			q = q.next
		}
	}
	
	def notifyRemoveIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
		var q = constraints;
		while (q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventRemoveValueIdx(q.cons,q.x,q.idx,v+q.delta));
			}
			q = q.next
		}
	}
	
	def notifyUpdateMinL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
		var q = constraints;
		while(q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventUpdateMin(q.cons,q.x,v+q.delta));
			}
			q = q.next;
		}
	}
	
	def notifyUpdateMinIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
		var q = constraints;
		while (q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventUpdateMinIdx(q.cons,q.x,q.idx,v+q.delta));
			}
			q = q.next;
		}
	}
	
	def notifyBindL1(constraints: PropagEventQueueVarInt, x: CPVarInt) {
		var q = constraints;
		while (q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventBindToValue(q.cons,q.x))
			}
			q = q.next
		}
	}
	
	def notifyBindIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt ) {
		var q = constraints;
		while (q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventBindToValueIdx(q.cons,q.x,q.idx))
			}
			q = q.next
		}
	}	
	
	def notifyUpdateMaxL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
		var q = constraints
		while (q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventUpdateMax(q.cons,q.x,v+q.delta));
			}
			q = q.next
		}
	}
	
	def notifyUpdateMaxIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
		var q = constraints;
		while (q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventUpdateMaxIdx(q.cons,q.x,q.idx,v+q.delta));
			}
			q = q.next
		}
	}
	
	def notifyUpdateBoundsL1(constraints: PropagEventQueueVarInt, x: CPVarInt) {
		var q = constraints;
		while (q != null) {
			if (q.cons.isActive()) {
				addQueueL1(new PropagEventUpdateBounds(q.cons,q.x));
			}
			q = q.next
		}
	}
	
	def notifyUpdateBoundsIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt) {
		var q = constraints;
		while (q != null) {
			if (q.cons.isActive()) {
				// we must do q.getVar rather than var because it might be a view of the var that did the domain modif
				addQueueL1(new PropagEventUpdateBoundsIdx(q.cons,q.x,q.idx)); 
			}
			q = q.next
		}
	}
	
	/**
	 * call only the propagate method of the constraints and trigger the fix point does not post it
	 * @param c
	 */
	def  propagate(c: Constraint*): CPOutcome = {
		if (status.getValue() == CPOutcome.Failure) return status.getValue();
		//assert(status.getValue() != CPOutcome.Failure);
		for (cons <- c) {
			if (cons.propagate() == CPOutcome.Failure) {
				return CPOutcome.Failure;
			}
		}
		if (propagate() == CPOutcome.Failure) {
			status.setValue(CPOutcome.Failure);
		}
		return status.getValue();
	}
	
	def addCutConstraints() {
		for (c <- cutConstraints) {
			propagQueueL2(c.priorityL2).add(c);
		}
	}
	
   val q1 = propagQueueL1.reverse
   val q2 = propagQueueL2.reverse
	
    /**
     * Fix Point algorithm
     * @return Failure is the fix point detects a failure that is one of the domain became empty, Suspend otherwise
     */
   
	protected def propagate(): CPOutcome = {
		assert(status.value != Failure);
		
		val t0 = System.currentTimeMillis();
		inPropagate = true;
		var ok = CPOutcome.Suspend;
		addCutConstraints();
		var fixed = false
		while (ok != Failure && !fixed) {
			//var p = int p;
		    if (q1.exists(!_.isEmpty())) {
		      val queue = q1.find(!_.isEmpty()).get
		      ok = queue.removeFirst().propagate()
		    } else if (q2.exists(!_.isEmpty())) {
		      val queue = q2.find(!_.isEmpty()).get
		      ok = queue.removeFirst().execute()
		    } else {
		      fixed = true
		    }
		}
		inPropagate = false;
		timeInFixPoint += System.currentTimeMillis()-t0;
		status.value = if (ok == Failure) ok else Suspend
		status.value
	}
   
    /**
     * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c
     * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint).
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
	def post(c: Constraint, st: CPPropagStrength): CPOutcome = {
		if (status.getValue() == Failure) return Failure; 
		var oc = c.setup(st);
		if (oc != Failure) {
			if (oc == Success) {
				c.deactivate();
			}
			//don't forget that posting a constraint can also post other constraints (e.g. reformulation)
			//so we must propagate because the queues may not be empty
			// we also check that posting this new constraint does not come from the propagate method otherwise we might have infinite recurtion
			if (!inPropagate) oc = propagate();
		}
		if (oc == Failure) {
			cleanQueues();
		}
		status.value = oc;
		return status.value;
	}
	
	def post(c: Constraint): CPOutcome = post(c,CPPropagStrength.Weak)
	
	def postCut(c: Constraint, st: CPPropagStrength = CPPropagStrength.Weak): CPOutcome = {
		val ok = post(c,st);
		cutConstraints.add(c);
		return ok;
	}

	
    /**
     * Add a constraint b == true to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c, the constraint
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise
     */
	def post(b: CPVarBool): CPOutcome  = post(new Eq(b, 1), CPPropagStrength.Weak)

    /**
     * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
     * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
     * @param constraints
     * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
	def post(constraints: Array[Constraint],st: CPPropagStrength): CPOutcome = {
		if (status.value == Failure) return Failure;
		var oc = Suspend
		for (c <- constraints) {
			oc = c.setup(st)
			if (oc == Success) {
				c.deactivate()
			}
			status.value = oc
			if (oc == Failure) {
				cleanQueues()
				return oc
			}
		}
		oc = propagate()
		if (oc == CPOutcome.Failure) {
			cleanQueues()
		}
		status.value = oc
		status.value
	}
	
	def post(constraints: Array[Constraint]): CPOutcome = post(constraints, CPPropagStrength.Weak);
	
    /**
     * Add a set of constraints to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm afterwards.
     * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
     * @param constraints
     * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
	def post(constraints: Collection[Constraint],st: CPPropagStrength): CPOutcome = post(constraints.map(x => x.asInstanceOf[Constraint]).toArray,st)

    def post(constraints: Collection[Constraint]): CPOutcome = post(constraints.map(x => x.asInstanceOf[Constraint]),CPPropagStrength.Weak)

	
    /**
     * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c
     * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint).
     * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
     */
    def add(c: Constraint, st: CPPropagStrength): CPOutcome = {
    	val res = post(c,st);
    	if ((res == Failure || status.value == Failure) && throwNoSolExceptions) {
        	throw  new NoSolutionException("the store failed when adding constraint :"+c);
        }
        return res;
    }
	
	def add(c: Constraint): CPOutcome = add(c,CPPropagStrength.Weak)
    /**
     * Add a constraint to the store (b == true) in a reversible way and trigger the fix-point algorithm. <br>
     * In a reversible way means that the constraint is present in the store only for descendant nodes.
     * @param c
     * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
     */
    def add(b: CPVarBool): CPOutcome = {
    	val res = post(new Eq(b, 1));
    	if ((res == Failure || status.value == Failure) && throwNoSolExceptions) {
        	throw  new NoSolutionException("the store failed when setting boolvar to true");
        }
        return res;
    }
    
    /**
     * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
     * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
     * @param constraints
     * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
     * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
     */
    def add(constraints: Collection[Constraint],st: CPPropagStrength): CPOutcome = {
    	val res = post(constraints,st);
    	if ((res == Failure || status.value == Failure) && throwNoSolExceptions) {
        	throw  new NoSolutionException("the store failed when adding constraints :"+constraints);
        }
        return res;
    }
    
    def add(constraints: Collection[Constraint]): CPOutcome = add(constraints,CPPropagStrength.Weak)
    
     def addCut(c: Constraint): CPOutcome = {
    	val res = postCut(c);
    	if ((res == Failure || status.value == Failure) && throwNoSolExceptions) {
        	throw new NoSolutionException("the store failed when adding constraint :"+c);
        }
        res;
    }    


}

object CPStore {
    /**
     * The highest priority for an Level 1 filtering method (the lowest priority is 0)
     */
	val MAXPRIORL1 = 2;
    /**
     * The highest priority for the propagate method i.e. L2 (the lowest priority is 0)
     */
	val MAXPRIORL2 = 7;
}
