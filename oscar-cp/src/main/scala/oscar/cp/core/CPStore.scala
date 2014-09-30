/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.core;

import java.util.Collection
import java.util.LinkedList
import scala.collection.JavaConversions.asJavaCollection
import scala.collection.JavaConversions.collectionAsScalaIterable
import oscar.algo.ArrayQueue
import oscar.algo.reversible.ReversiblePointer
import oscar.algo.search.SearchNode
import oscar.cp.constraints.Eq
import oscar.cp.core.CPOutcome.Failure
import oscar.cp.core.CPOutcome.Success
import oscar.cp.core.CPOutcome.Suspend
import oscar.cp.constraints.EqCons

/**
 * Constraint Programming CPStore
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPStore(val propagStrength: CPPropagStrength) extends SearchNode {

  def this() = this(CPPropagStrength.Weak)
  
  import oscar.algo.ArrayQueue // custom array-based double ended queue
  private val propagQueueL1 = Array.fill(CPStore.MaxPriorityL1 + 1)(new ArrayQueue[() => CPOutcome](1000))
  private val propagQueueL2 = Array.fill(CPStore.MaxPriorityL2 + 1)(new ArrayQueue[Constraint](100))
  private val cutConstraints = new ArrayQueue[Constraint](1) // usually empty

  // Status of the store
  private val status: ReversiblePointer[CPOutcome] = new ReversiblePointer[CPOutcome](this, Suspend)

  // Number of calls to propagate method in any constraints
  private var nbPropag = 0
  
  // Total time spent in the fix point algorithm
  private var timeInFixPoint: Long = 0

  // True if the store is in the fix point algorithm
  private var inPropagate = false
  
  // Reference to the last constraint called
  private var lastConstraint: Constraint = null

  // Use for fast access to priority queue
  private var highestPriorL1 = 0
  private var highestPriorL2 = 0;

  // True if the L1 queue is empty
  private var isL1QueueEmpty = true

  /**
   *  Returns the last constraint called in the propagate algorithm.
   *
   *  Note that `null` is returned if no constraint has been called.
   *
   *  @return The last constraint called by the propagate algorithm.
   */
  def lastConstraintCalled: Constraint = lastConstraint

  /** Changes the status of the store to Failure */
  override def fail(): Unit = status.setValue(Failure)

  /** Returns true if the store is failed */
  override def isFailed: Boolean = status.value == Failure

  // Cleans the propagation queues
  @inline private def cleanQueues(): Unit = {
    var i = 0
    // Clean queue L1
    while (i < propagQueueL1.length) {
      propagQueueL1(i).clear()
      i += 1
    }
    i = 0
    // Clean queue L2
    while (i < propagQueueL2.length) {
      propagQueueL2(i).clear()
      i += 1
    }
  }

  // Adds the constraint in the L2 queue
  @inline protected def addQueueL2(c: Constraint): Unit = {
    if (c.isActive && !c.isInQueue && (!c.inPropagate || !c.idempotent)) {
      c.setInQueue()
      val priority = c.priorityL2
      propagQueueL2(priority).addLast(c)
      if (priority > highestPriorL2) {
        highestPriorL2 = priority
      }
    }
  }

  /**
   * Notify the constraints that is enqueue them in the L2 propagation queue such that their propagate method
   * is called at some point in the current fix point
   * @param constraints
   */
  def notifyL2(constraints: ConstraintQueue): Unit = {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      addQueueL2(c)
      q = q.next
    }
  }

  @inline protected def addQueueL1(c: Constraint, prior: Int, evt: => CPOutcome): Unit = {
    propagQueueL1(prior).addLast(() =>
      if (c.isActive) {
        lastConstraint = c // last constraint called
        val oc = evt
        if (oc == Success) c.deactivate()
        oc
      } else Suspend);
    isL1QueueEmpty = false
    highestPriorL1 = Math.max(highestPriorL1, prior);
  }

  def notifRemoveL1(constraints: PropagEventQueueVarInt[CPIntVar], x: CPIntVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      if (c.isActive) {
        addQueueL1(c, c.priorityRemoveL1, c.valRemove(x, x.transform(v)));
      }
      q = q.next
    }
  }

  def notifyRemoveIdxL1(constraints: PropagEventQueueVarInt[CPIntVar], x: CPIntVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        addQueueL1(c, c.priorityRemoveL1, c.valRemoveIdx(x, idx, x.transform(v)))
      }
      q = q.next
    }
  }

  def notifyUpdateBoundsL1(constraints: PropagEventQueueVarInt[CPIntervalVar], x: CPIntervalVar) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      if (c.isActive) {
        addQueueL1(c, c.priorityBoundsL1, c.updateBounds(x))
      }
      q = q.next;
    }
  }

  def notifyUpdateBoundsIdxL1(constraints: PropagEventQueueVarInt[CPIntervalVar], x: CPIntervalVar) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        addQueueL1(c, c.priorityBoundsL1, c.updateBoundsIdx(x, idx))
      }
      q = q.next;
    }
  }

  def notifyBindL1(constraints: PropagEventQueueVarInt[CPIntervalVar], x: CPIntervalVar) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      if (c.isActive) {
        addQueueL1(c, c.priorityBindL1, c.valBind(x))
      }
      q = q.next
    }
  }

  def notifyBindIdxL1(constraints: PropagEventQueueVarInt[CPIntervalVar], x: CPIntervalVar) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        addQueueL1(c, c.priorityBindL1, c.valBindIdx(x, idx))
      }
      q = q.next
    }
  }

  // set variable

  def notifyRequired(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        addQueueL1(c, c.priorityBindL1, c.valRequired(x, v))
      }
      q = q.next
    }
  }

  def notifyRequiredIdx(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        addQueueL1(c, c.priorityBindL1, c.valRequiredIdx(x, idx, v))
      }
      q = q.next
    }
  }

  def notifyExcluded(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        addQueueL1(c, c.priorityBindL1, c.valExcluded(x, v))
      }
      q = q.next
    }
  }

  def notifyExcludedIdx(constraints: PropagEventQueueVarSet, x: CPSetVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      val idx = q.idx
      if (c.isActive) {
        addQueueL1(c, c.priorityBindL1, c.valExcludedIdx(x, idx, v))
      }
      q = q.next
    }
  }

  /**
   *  Call the propagate function of the constraints and trigger the fix point algorithm
   *  
   *  Note that the constraints are not added to the model

   *  @param constraints a sequence of constraints
   */
  def propagate(constraints: Constraint*): CPOutcome = {
    if (status.value == Failure) Failure
    else {
      constraints.foreach(c => addQueueL2(c))
      propagate()
      status.value // may be changed by propagate()
    }
  }

  protected def propagate(): CPOutcome = {
    if (status.value == Failure) throw new RuntimeException("propagate on a failed store")
    else {
      val t0 = System.currentTimeMillis()

      // Adds the cut constraints
      cutConstraints.foreach(c => {
        if (c.isActive) {
          c.setInQueue()
          propagQueueL2(c.priorityL2).addLast(c)
        }
      })

      // Initializes the fix-point algorithm
      var ok = Suspend
      var fixed = false
      inPropagate = true
      highestPriorL1 = CPStore.MaxPriorityL1
      highestPriorL2 = CPStore.MaxPriorityL2

      while (ok != Failure && !fixed) {

        var p = highestPriorL1

        // Propagate queue L1
        while (!isL1QueueEmpty && ok != Failure) {

          p = highestPriorL1

          // Adjust the L1 priority
          while (p >= 0 && propagQueueL1(p).isEmpty) p -= 1

          if (p < 0) isL1QueueEmpty = true
          else {
            highestPriorL1 = p
            while (highestPriorL1 <= p && !propagQueueL1(p).isEmpty && ok != Failure) {
              val event = propagQueueL1(p).removeFirst()

              isL1QueueEmpty = (p == 0 && propagQueueL1(p).isEmpty)
              highestPriorL1 = p
              // Execute the event
              ok = event()
            }
          }
        }

        p = highestPriorL2

        // Adjust the L2 priority
        while (p >= 0 && propagQueueL2(p).isEmpty) p -= 1

        if (p < 0) fixed = true
        else {
          highestPriorL2 = p
          while (highestPriorL2 <= p && isL1QueueEmpty && !propagQueueL2(p).isEmpty && ok != Failure) {
            val c = propagQueueL2(p).removeFirst()
            lastConstraint = c
            highestPriorL2 = p
            nbPropag += 1
            ok = c.execute()
          }
        }
      }

      inPropagate = false
      timeInFixPoint += System.currentTimeMillis() - t0

      if (ok != Failure) Suspend
      else {
        cleanQueues() // May be not empty
        status.value = Failure
        Failure
      }
    }
  }

  def printQueues(): Unit = {
    println("----------")
    propagQueueL1.foreach(q => println("L1: " + q.size))
    propagQueueL2.foreach(q => println("L2: " + q.size))
  }

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
      // Don't forget that posting a constraint can also post other constraints (e.g. reformulation)
      // so we must propagate because the queues may not be empty
      // we also check that posting this new constraint does not come from the propagate method otherwise we might have infinite recurtion
      if (!inPropagate) oc = propagate();
    }
    status := oc
    status.value
  }

  def post(c: Constraint): CPOutcome = post(c, propagStrength)

  def postCut(c: Constraint): CPOutcome = postCut(c, propagStrength)

  def postCut(c: Constraint, st: CPPropagStrength): CPOutcome = {
    val ok = post(c, st);
    cutConstraints.addLast(c);
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
  def post(b: CPBoolVar): CPOutcome = post(new EqCons(b, 1), propagStrength)

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
        return oc
      }
    }
    oc = propagate()
    status.value = oc
    status.value
  }

  def post(constraints: Array[Constraint]): CPOutcome = post(constraints, propagStrength);

  /**
   * Add a set of constraints to the store (with a Weak propagation strength) in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @return Failure if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def post(constraints: Collection[Constraint], st: CPPropagStrength): CPOutcome = post(constraints.map(x => x.asInstanceOf[Constraint]).toArray, st)

  def post(constraints: Collection[Constraint]): CPOutcome = post(constraints.map(x => x.asInstanceOf[Constraint]), propagStrength)

  def assign(x: CPIntVar, v: Int): CPOutcome = {
    if (status.getValue() == Failure) return Failure
    var oc = x.assign(v)
    if (oc != Failure) {
      oc = propagate()
    }
    status.value = oc
    return status.value
  }

  def remove(x: CPIntVar, v: Int): CPOutcome = {
    if (status.getValue() == Failure) return Failure
    var oc = x.removeValue(v)
    if (oc != Failure) {
      oc = propagate()
    }
    status.value = oc
    return status.value
  }
  
  def add(c: Constraint, st: CPPropagStrength): CPOutcome = post(c, st)

  def add(c: Constraint): CPOutcome = add(c, propagStrength)

  def add(b: CPBoolVar): CPOutcome = post(new EqCons(b, 1))


  /**
   * Add a set of constraints to the store in a reversible way and trigger the fix-point algorithm afterwards.
   * In a reversible way means that the posted constraints are present in the store only for descendant nodes.
   * @param constraints
   * @param st the propagation strength asked for the constraint. Will be used only if available for the constraint (see specs of the constraint)
   * @throws NoSolutionException if the fix point detects a failure that is one of the domain became empty, Suspend otherwise.
   */
  def add(constraints: Collection[Constraint], st: CPPropagStrength): CPOutcome = post(constraints, st);

  def addCut(c: Constraint): CPOutcome = postCut(c)

  def add(constraints: Collection[Constraint]): CPOutcome = add(constraints, propagStrength)

  def add(constraints: Iterable[Constraint], st: CPPropagStrength): CPOutcome = {
    val cs = new LinkedList[Constraint]()
    constraints.foreach(cs.add(_))
    add(cs, st)
  }

  def add(constraints: Iterable[Constraint]): CPOutcome = add(constraints, propagStrength)

  def +=(c: Constraint, st: CPPropagStrength): CPOutcome = add(c, st)
  def +=(c: Constraint): CPOutcome = add(c, propagStrength)
}

object CPStore {

  /** The highest priority for an Level 1 filtering method */
  @deprecated val MAXPRIORL1 = 2

  /** The highest priority for the propagate method i.e. L2 */
  @deprecated val MAXPRIORL2 = 7

  /** The highest priority for an Level 1 filtering method */
  val MaxPriorityL1 = 2

  /** The lowest priority for an Level 1 filtering method */
  val MinPriorityL1 = 0

  /** The highest priority for an Level 2 filtering method */
  val MaxPriorityL2 = 7

  /** The lowest priority for an Level 2 filtering method */
  val MinPriorityL2 = 0
}
