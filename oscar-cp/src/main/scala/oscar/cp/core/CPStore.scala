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
import oscar.algo.reversible.ReversiblePointer;
import oscar.algo.search.SearchNode;
import collection.JavaConversions._

import oscar.cp.core.CPOutcome._

/**
 * Constraint Programming CPStore
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPStore extends SearchNode {
  /**
   * Number of call to propagate method in any constraints
   */
  private var nbPropag = 0

  val propagQueueL1 = Array.fill(CPStore.MAXPRIORL1 + 1)(new LinkedList[() => CPOutcome]())
  val propagQueueL2 = Array.fill(CPStore.MAXPRIORL2 + 1)(new LinkedList[Constraint]())

  var status: ReversiblePointer[CPOutcome] = new ReversiblePointer[CPOutcome](this, Suspend);

  /**
   * The total time spent in the fix point algorithm (usually the significant part)
   */
  var timeInFixPoint: Long = 0

  var inPropagate = false

  val cutConstraints = new LinkedList[Constraint]()

  var throwNoSolExceptions = true

  private var highestPriorL1 = 0
  private var highestPriorL2 = 0;

  private var isL1QueueEmpty = true

  override def fail() { status.setValue(Failure) }

  /**
   * deactivate the no solution exception when an add is used and an inconsistent model is detected
   */
  def deactivateNoSolExceptions() {
    throwNoSolExceptions = false;
  }

  override def isFailed: Boolean = status.value == CPOutcome.Failure;

  private def cleanQueues(): Unit = {
    propagQueueL1.foreach(_.clear())
    propagQueueL2.foreach(_.clear())
  }

  def addQueueL2(c: Constraint): Int = {
    //println(c+" active:"+c.isActive()+" inqueue:"+c.isInQueue() + " in propagate"+ c.inPropagate()+ " indempotent:"+c.idempotent)
    if ((c.isActive() && !c.isInQueue()) && (!c.inPropagate() || !c.idempotent)) {
      c.setInQueue()
      propagQueueL2(c.priorityL2).add(c)
      c.priorityL2
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
    //println("constraints before notifyL2:"+constraints)
    while (q != null) {

      val c = q.cons;
      //println("add constraint "+c+" on L2 queue")
      val p = addQueueL2(c);
      highestPriorL2 = Math.max(p, highestPriorL2);
      q = q.next
    }
    //println("constraints after notifyL2:"+constraints)
  }

  private def addQueueL1(c: Constraint, prior: Int, evt: => CPOutcome) {
    propagQueueL1(prior).add(() =>
      if (c.isActive) {
        val oc = evt;
        if (oc == Success) c.deactivate();
        oc
      } else Suspend);
    isL1QueueEmpty = false
    highestPriorL1 = Math.max(highestPriorL1, prior);
  }

  def notifRemoveL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val delta = q.delta
      if (c.isActive()) {
        addQueueL1(c, c.priorityRemoveL1, c.valRemove(x, v + delta));
      }
      q = q.next
    }
  }

  def notifyRemoveIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      val delta = q.delta
      if (c.isActive()) {
        addQueueL1(c, c.priorityRemoveL1, c.valRemoveIdx(x, idx, v + delta))
      }
      q = q.next
    }
  }

  def notifyUpdateMinL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      val delta = q.delta
      if (c.isActive()) {
        addQueueL1(c, c.priorityBoundsL1, c.updateMin(x, v + delta))
      }
      q = q.next;
    }
  }

  def notifyUpdateMinIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      val delta = q.delta
      if (c.isActive()) {
        addQueueL1(c, c.priorityBoundsL1, c.updateMinIdx(x, idx, v + delta));
      }
      q = q.next;
    }
  }

  def notifyUpdateMaxL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val delta = q.delta
      if (c.isActive()) {
        addQueueL1(c, c.priorityBoundsL1, c.updateMax(x, v + delta))
      }
      q = q.next;
    }
  }

  def notifyUpdateMaxIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      val delta = q.delta
      if (c.isActive()) {
        addQueueL1(c, c.priorityBoundsL1, c.updateMaxIdx(x, idx, v + delta));
      }
      q = q.next;
    }
  }

  def notifyUpdateBoundsL1(constraints: PropagEventQueueVarInt, x: CPVarInt) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      if (c.isActive()) {
        addQueueL1(c, c.priorityBoundsL1, c.updateBounds(x))
      }
      q = q.next;
    }
  }

  def notifyUpdateBoundsIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive()) {
        addQueueL1(c, c.priorityBoundsL1, c.updateBoundsIdx(x, idx))
      }
      q = q.next;
    }
  }

  def notifyBindL1(constraints: PropagEventQueueVarInt, x: CPVarInt) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      if (c.isActive()) {
        addQueueL1(c, c.priorityBindL1, c.valBind(x))
      }
      q = q.next
    }
  }

  def notifyBindIdxL1(constraints: PropagEventQueueVarInt, x: CPVarInt) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive()) {
        addQueueL1(c, c.priorityBindL1, c.valBindIdx(x, idx))
      }
      q = q.next
    }
  }

  // set variable

  def notifyRequired(constraints: PropagEventQueueVarSet, x: CPVarSet, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive()) {
        addQueueL1(c, c.priorityBindL1, c.valRequired(x, v))
      }
      q = q.next
    }
  }

  def notifyRequiredIdx(constraints: PropagEventQueueVarSet, x: CPVarSet, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive()) {
        addQueueL1(c, c.priorityBindL1, c.valRequiredIdx(x, idx, v))
      }
      q = q.next
    }
  }

  def notifyExcluded(constraints: PropagEventQueueVarSet, x: CPVarSet, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive()) {
        addQueueL1(c, c.priorityBindL1, c.valExcluded(x, v))
      }
      q = q.next
    }
  }

  def notifyExcludedIdx(constraints: PropagEventQueueVarSet, x: CPVarSet, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive()) {
        addQueueL1(c, c.priorityBindL1, c.valExcludedIdx(x, idx, v))
      }
      q = q.next
    }
  }

  /**
   * call only the propagate method of the constraints and trigger the fix point does not post it
   * @param c
   */
  def propagate(c: Constraint*): CPOutcome = {
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
    for (c <- cutConstraints; if c.isActive) {
      c.setInQueue()
      propagQueueL2(c.priorityL2).add(c);
    }
  }

  val q1 = propagQueueL1.reverse
  val q2 = propagQueueL2.reverse

  private def printQueue() {
    println("--- L1 queue ---")
    q1.foreach(q => println(q.mkString("[", ",", "]")))
    println("--- L2 queue ---")
    q2.foreach(q => println(q.mkString("[", ",", "]")))
    println("---")
  }

  /**
   * Fix Point algorithm
   * @return Failure is the fix point detects a failure that is one of the domain became empty, Suspend otherwise
   */

  protected def propagate3(): CPOutcome = {
    assert(status.value != Failure)
    val t0 = System.currentTimeMillis();
    inPropagate = true
    var ok = CPOutcome.Suspend
    addCutConstraints()
    var fixed = false
    while (ok != Failure && !fixed) {
      if (q1.exists(!_.isEmpty())) {
        val queue = q1.find(!_.isEmpty()).get
        ok = queue.removeFirst()()
      } else if (q2.exists(!_.isEmpty())) {
        val queue = q2.find(!_.isEmpty()).get
        ok = queue.removeFirst().execute()
      } else {
        fixed = true
      }
    }
    inPropagate = false
    timeInFixPoint += System.currentTimeMillis() - t0
    status.value = if (ok == Failure) ok else Suspend
    status.value
  }

  protected def propagate(): CPOutcome = {
    //println("------propagate------")
    assert(status.value != Failure)
    val t0 = System.currentTimeMillis();
    inPropagate = true
    var ok = CPOutcome.Suspend
    addCutConstraints()
    var fixed = false
    highestPriorL1 = CPStore.MAXPRIORL1
    highestPriorL2 = CPStore.MAXPRIORL2

    while (ok != CPOutcome.Failure && !fixed) {
      //printQueues()
      var p = highestPriorL1
      while (!isL1QueueEmpty && ok != CPOutcome.Failure) {
        //print("*")
        //printQueues()
        p = highestPriorL1
        while (p >= 0 && propagQueueL1(p).isEmpty()) p -= 1;
        if (p < 0) isL1QueueEmpty = true
        else {
          highestPriorL1 = p
          while (highestPriorL1 <= p && !propagQueueL1(p).isEmpty() && ok != CPOutcome.Failure) {
            //print("_")
            var event = propagQueueL1(p).removeFirst()
            isL1QueueEmpty = (p == 0 && propagQueueL1(p).isEmpty())
            highestPriorL1 = p
            ok = event()
          }
        }

      }

      p = highestPriorL2
      while (p >= 0 && propagQueueL2(p).isEmpty()) p -= 1;
      if (p < 0) fixed = true
      else {
        highestPriorL2 = p
        while (highestPriorL2 <= p && isL1QueueEmpty && !propagQueueL2(p).isEmpty() && ok != CPOutcome.Failure) {
          //print(".")
          val c = propagQueueL2(p).removeFirst();
          highestPriorL2 = p;
          nbPropag += 1;
          ok = c.execute();
        }
      }

    }
    //println("--------------->fixed")
    inPropagate = false
    timeInFixPoint += System.currentTimeMillis() - t0
    status.value = if (ok == Failure) ok else Suspend
    status.value
  }

  def printQueues() = {
    println("----------")
    for (q <- propagQueueL1)
      println("L1:" + q.size())
    for (q <- propagQueueL2)
      println("L2:" + q.size())
  }

  /*
    /**
     * Fix Point algorithm
     * @return Failure is the fix point detects a failure that is one of the domain became empty, Suspend otherwise
     */
        protected CPOutcome propagate() {
                assert(status.getValue() != CPOutcome.Failure);
                
                long t0 = System.currentTimeMillis();
                inPropagate = true;
                CPOutcome ok = objective.propagate();
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
                        if (p < 0) break;
                        while (ok != CPOutcome.Failure && !propagQueueL2[p].isEmpty()) {
                                Constraint c = propagQueueL2[p].removeFirst();
                                highestPriorL2 = p;
                                nbPropag++;
                                ok = c.execute();
                                if (highestPriorL2 > p || !isL1QueueEmpty()) break;
                        }
                }
                inPropagate = false;
                timeInFixPoint += System.currentTimeMillis()-t0;
                status.setValue( ok == CPOutcome.Failure ? ok : CPOutcome.Suspend);
                return ok == CPOutcome.Failure ? ok : CPOutcome.Suspend;
        }
        
        private boolean isL1QueueEmpty() {
                for(int i = 0; i <= MAXPRIORL1; i++) {
                        if (!propagQueueL1[i].isEmpty()) return false;
                }
                return true;
        }
	 */

  /**
   * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param c
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint).
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def post(c: Constraint, st: CPPropagStrength): CPOutcome = {
    if (status.value == Failure) return Failure;
    var oc = c.setup(st)
    if (oc != Failure) {
      if (oc == Success) {
        c.deactivate()
      }
      //don't forget that posting a constraint can also post other constraints (e.g. reformulation)
      //so we must propagate because the queues may not be empty
      // we also check that posting this new constraint does not come from the propagate method otherwise we might have infinite recurtion
      if (!inPropagate) oc = propagate();
    }
    if (oc == Failure) {
      cleanQueues()
    }
    status := oc
    return status.value
  }

  def post(c: Constraint): CPOutcome = post(c, CPPropagStrength.Weak)

  def postCut(c: Constraint, st: CPPropagStrength = CPPropagStrength.Weak): CPOutcome = {
    val ok = post(c, st);
    cutConstraints.add(c);
    return ok;
  }
  
  def resetCuts(): Unit = {
    for (c <- cutConstraints) {
      c.deactivate() // we cannot really remove them because they were set-up
    }
    cutConstraints.clear()
  }
  

  /**
   * Add a constraint b == true to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param c, the constraint
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise
   */
  def post(b: CPVarBool): CPOutcome = post(new Eq(b, 1), CPPropagStrength.Weak)

  /**
   * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def post(constraints: Array[Constraint], st: CPPropagStrength): CPOutcome = {
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
  def post(constraints: Collection[Constraint], st: CPPropagStrength): CPOutcome = post(constraints.map(x => x.asInstanceOf[Constraint]).toArray, st)

  def post(constraints: Collection[Constraint]): CPOutcome = post(constraints.map(x => x.asInstanceOf[Constraint]), CPPropagStrength.Weak)

  /**
   * Add a constraint to the store in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param c
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint).
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
   */
  def add(c: Constraint, st: CPPropagStrength): CPOutcome = {
    val res = post(c, st);
    if ((res == Failure || status.value == Failure) && throwNoSolExceptions) {
      throw new NoSolutionException("the store failed when adding constraint :" + c);
    }
    return res;
  }

  def add(c: Constraint): CPOutcome = add(c, CPPropagStrength.Weak)
  /**
   * Add a constraint to the store (b == true) in a reversible way and trigger the fix-point algorithm. <br>
   * In a reversible way means that the constraint is present in the store only for descendant nodes.
   * @param c
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty
   */
  def add(b: CPVarBool): CPOutcome = {
    val res = post(new Eq(b, 1));
    if ((res == Failure || status.value == Failure) && throwNoSolExceptions) {
      throw new NoSolutionException("the store failed when setting boolvar to true");
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
  def add(constraints: Collection[Constraint], st: CPPropagStrength): CPOutcome = {
    val res = post(constraints, st);
    if ((res == Failure || status.value == Failure) && throwNoSolExceptions) {
      throw new NoSolutionException("the store failed when adding constraints :" + constraints);
    }
    return res;
  }

  def add(constraints: Collection[Constraint]): CPOutcome = add(constraints, CPPropagStrength.Weak)
  
  def add(constraints: Iterable[Constraint], st: CPPropagStrength = CPPropagStrength.Weak): CPOutcome = {
    val cs = new LinkedList[Constraint]()
    constraints.foreach(cs.add(_))
    add(cs, st)
  }

  def addCut(c: Constraint): CPOutcome = {
    val res = postCut(c);
    if ((res == Failure || status.value == Failure) && throwNoSolExceptions) {
      throw new NoSolutionException("the store failed when adding constraint :" + c);
    }
    res;
  }
  
  
  
  def assign(x: CPVarInt, v: Int): CPOutcome = {
    if (status.getValue() == Failure) return Failure
    var oc = x.assign(v)
    if (oc != Failure) {
      oc = propagate()
    }
    cleanQueues()
    status.value = oc
    return status.value
  }
  
  def remove(x: CPVarInt, v: Int): CPOutcome = {
    if (status.getValue() == Failure) return Failure
    var oc = x.removeValue(v)
    if (oc != Failure) {
      oc = propagate()
    }
    cleanQueues()
    status.value = oc
    return status.value
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
