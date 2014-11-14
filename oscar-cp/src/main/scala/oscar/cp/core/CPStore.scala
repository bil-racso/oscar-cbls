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

package oscar.cp.core

import java.util.{Collection, LinkedList}

import oscar.algo.ArrayQueue
import oscar.algo.reversible.ReversiblePointer
import oscar.algo.search.SearchNode
import oscar.cp.constraints.EqCons
import oscar.cp.core.CPOutcome.{Failure, Success, Suspend}

import scala.collection.JavaConversions.{asJavaCollection, collectionAsScalaIterable}

/**
 * Constraint Programming CPStore
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPStore( final val propagStrength: CPPropagStrength) extends SearchNode {

  def this() = this(CPPropagStrength.Weak)

  // Propagation queue L1 (AC5)
  private val propagQueueL1 = Array.fill(CPStore.MaxPriorityL1 + 1)(new ArrayQueue[() => CPOutcome](1000))
  private var highestPriorL1 = -1

  // Propagation queue L2 (AC3)
  private val propagQueueL2 = Array.fill(CPStore.MaxPriorityL2 + 1)(new ArrayQueue[Constraint](100))
  private var highestPriorL2 = -1

  private val cutConstraints = new ArrayQueue[Constraint](1) // usually empty

  // Status of the store (should be replaced by the failed reversible boolean of SearchNode)
  private val status: ReversiblePointer[CPOutcome] = new ReversiblePointer[CPOutcome](this, Suspend)

  // Total time spent in the fixed point algorithm
  private var timeInFixedPoint: Long = 0

  // True if the store is executing the fixed point algorithm
  private var inFixedPoint = false

  // Reference to the last constraint called
  private var lastConstraint: Constraint = null

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
    // Clean queue L1
    highestPriorL1 = -1
    var i = 0
    while (i < propagQueueL1.length) {
      propagQueueL1(i).clear()
      i += 1
    }
    // Clean queue L2
    highestPriorL2 = -1
    i = 0
    while (i < propagQueueL2.length) {
      propagQueueL2(i).clear()
      i += 1
    }
  }

  // Adds the constraint in the L2 queue
  @inline protected def enqueueL2(c: Constraint): Unit = {
    if (c.isActive && !c.isInQueue && (!c.inPropagate || !c.idempotent)) {
      c.setInQueue()
      val priority = c.priorityL2
      propagQueueL2(priority).addLast(c)
      if (priority > highestPriorL2) {
        highestPriorL2 = priority
      }
    }
  }

  // Adds the constraint in the L1 queue
  @inline protected def enqueueL1(c: Constraint, priority: Int, evt: => CPOutcome): Unit = {
    propagQueueL1(priority).addLast(() => {
      if (c.isActive) {
        lastConstraint = c // last constraint called
        val oc = evt
        if (oc == Success) c.deactivate()
        oc
      } else Suspend
    })
    if (priority > highestPriorL1) {
      highestPriorL1 = priority
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
      enqueueL2(c)
      q = q.next
    }
  }

  def notifRemoveL1(constraints: PropagEventQueueVarInt[CPIntVar], x: CPIntVar, v: Int) {
    var q = constraints;
    while (q != null) {
      val c = q.cons
      val x = q.x
      if (c.isActive) {
        enqueueL1(c, c.priorityRemoveL1, c.valRemove(x, x.transform(v)));
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
        enqueueL1(c, c.priorityRemoveL1, c.valRemoveIdx(x, idx, x.transform(v)))
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
        enqueueL1(c, c.priorityBoundsL1, c.updateBounds(x))
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
        enqueueL1(c, c.priorityBoundsL1, c.updateBoundsIdx(x, idx))
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
        enqueueL1(c, c.priorityBindL1, c.valBind(x))
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
        enqueueL1(c, c.priorityBindL1, c.valBindIdx(x, idx))
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
        enqueueL1(c, c.priorityBindL1, c.valRequired(x, v))
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
        enqueueL1(c, c.priorityBindL1, c.valRequiredIdx(x, idx, v))
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
        enqueueL1(c, c.priorityBindL1, c.valExcluded(x, v))
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
        enqueueL1(c, c.priorityBindL1, c.valExcludedIdx(x, idx, v))
      }
      q = q.next
    }
  }

  /**
   *  Call the propagate function of the constraints and trigger the fix point algorithm
   *
   *  Note that the constraints are not added to the model
   *
   *  @param constraints a sequence of constraints
   */
  def propagate(constraints: Constraint*): CPOutcome = {
    if (status.value == Failure) Failure
    else {
      constraints.foreach(c => enqueueL2(c))
      propagate()
      status.value // may be changed by propagate()
    }
  }

  protected def propagate(): CPOutcome = {
    if (status.value == Failure) throw Inconsistency("the store is already inconsistent")
    else {
      val t = System.currentTimeMillis()
      inFixedPoint = true
      try {
        val outcome = fixedPoint()
        if (outcome == Failure) {
          cleanQueues() // may be not empty
          status.value = Failure
        }
        outcome
      } catch {
        case i: Inconsistency => {
          cleanQueues() // may be not empty
          status.value = Failure
          Failure
        }
      } finally {
        timeInFixedPoint += System.currentTimeMillis() - t
        inFixedPoint = false
      }
    }
  }

  @inline private def fixedPoint(): CPOutcome = {

    // Adds the cut constraints
    cutConstraints.foreach(c => enqueueL2(c))

    var isFailed = false
    while (!isFailed && (highestPriorL1 >= 0 || highestPriorL2 >= 0)) {

      // Propagate L1
      while (highestPriorL1 >= 0 && !isFailed) {
        val queue = propagQueueL1(highestPriorL1)
        if (queue.isEmpty) highestPriorL1 -= 1
        else {
          val event = queue.removeFirst()
          isFailed = event() == Failure
        }
      }

      // Propagate L2 if no constraint in L1
      while (highestPriorL1 < 0 && highestPriorL2 >= 0 && !isFailed) {
        val queue = propagQueueL2(highestPriorL2)
        if (queue.isEmpty) highestPriorL2 -= 1
        else {
          val constraint = queue.removeFirst()
          lastConstraint = constraint
          isFailed = constraint.execute() == Failure
        }
      }
    }

    if (isFailed) Failure
    else Suspend
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
  def post(constraint: Constraint, st: CPPropagStrength): CPOutcome = {
    if (status.value == Failure) Failure
    else if (inFixedPoint) { // already in a try block
      val outcome = constraint.setup(st)
      if (outcome == Success) constraint.deactivate()
      status.value = outcome
      outcome
    } else try {
      val outcome = constraint.setup(st)
      if (outcome == Failure) {
        status.value = Failure
        Failure
      } else {
        if (outcome == Success) constraint.deactivate()
        propagate()
      }
    } catch {
      case i: Inconsistency => {
        status.value = Failure
        Failure
      }
    }
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
    if (status.value == Failure) Failure
    else if (inFixedPoint) { // already in a try block
      var outcome: CPOutcome = Suspend
      var i = 0
      while (i < constraints.length && outcome != Failure) {
        val constraint = constraints(i)
        outcome = constraint.setup(st)
        if (outcome == Success) constraint.deactivate()
        i += 1
      }
      status.value = outcome
      outcome
    } else try {
      var outcome: CPOutcome = Suspend
      var i = 0
      while (i < constraints.length && outcome != Failure) {
        val constraint = constraints(i)
        outcome = constraint.setup(st)
        if (outcome == Success) constraint.deactivate()
        i += 1
      }
      if (outcome != Failure) propagate()
      else {
        status.value = Failure
        Failure
      }
    } catch {
      case i: Inconsistency => {
        status.value = Failure
        Failure
      }
    }
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
    if (status.value == Failure) Failure
    else try {
      val outcome = x.assign(v)
      if (outcome != Failure) propagate()
      else {
        status.value = Failure
        Failure
      }
    } catch {
      case i: Inconsistency => {
        status.value = Failure
        Failure
      }
    }
  }

  def remove(x: CPIntVar, v: Int): CPOutcome = {
    if (status.value == Failure) Failure
    else try {
      val outcome = x.removeValue(v)
      if (outcome != Failure) propagate()
      else {
        status.value = Failure
        Failure
      }
    } catch {
      case i: Inconsistency => {
        status.value = Failure
        Failure
      }
    }
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
