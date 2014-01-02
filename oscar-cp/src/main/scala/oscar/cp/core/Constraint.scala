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

import oscar.algo.reversible.ReversibleBool
import oscar.cp.constraints.implementations.Garded
import scala.collection.mutable.ArrayBuffer


abstract class Snapshot {
  def update()
}

class SnapshotVarInt(x: CPVarInt) extends Snapshot {
  var oldMin: Int = x.min 
  var oldMax: Int = x.max
  var oldSize: Int = x.size
  def update() {
    oldMin = x.min
    oldMax = x.max
    oldSize = x.size
  }
}

class SnapshotVarSet(x: CPVarSet) extends Snapshot {
  var oldSizePossible: Int = x.possibleSize
  var oldSizeRequired: Int = x.requiredSize
  def update() {
    oldSizePossible = x.possibleSize
    oldSizeRequired = x.requiredSize
  }
}

/**
 * Abstract class extended by any CP constraints
 * @author Pierre Schaus pschaus@gmail.com
 */
abstract class Constraint(val s: CPStore, val name: String = "cons") {

  val active = new ReversibleBool(s,true)
  val inQueue = new ReversibleBool(s,false)

  val snapshotsVarInt = scala.collection.mutable.Map[CPVarInt, SnapshotVarInt]() 
  val snapshotsVarSet = scala.collection.mutable.Map[CPVarSet, SnapshotVarSet]()


  private var _mustSnapshot = false

  def addSnapshot(x: CPVarInt): Unit = {
    snapshotsVarInt(x) = new SnapshotVarInt(x)
    snapshotsVarInt(x).update()
    if (!_mustSnapshot) {
      s.onPop { snapShot() }
      _mustSnapshot = true
    } 
  }

  private def snapShot() {
    snapshotVarInt()
    snapshotVarSet()
  }

  protected def snapshotVarInt(): Unit = {
    if (snapshotsVarInt.size > 0) {
      snapshotsVarInt.values.foreach(_.update())
    }
  }
  

  def addSnapshot(x: CPVarSet): Unit = {
    snapshotsVarSet(x) = new SnapshotVarSet(x)
    snapshotsVarSet(x).update()
    if (!_mustSnapshot) {
      s.onPop { snapShot() }
      _mustSnapshot = true
    }    
  }

  protected def snapshotVarSet(): Unit = {
    if (snapshotsVarSet.size > 0)
      snapshotsVarSet.values.foreach(_.update())
  }  

  private var priorL2 = CPStore.MAXPRIORL2 - 2
  private var priorBindL1 = CPStore.MAXPRIORL1 - 1
  private var priorBoundsL1 = CPStore.MAXPRIORL1 - 2
  private var priorRemoveL1 = CPStore.MAXPRIORL1 - 2
  private var priorRequireL1 = CPStore.MAXPRIORL1 - 1
  private var priorExcludeL1 = CPStore.MAXPRIORL1 - 2

  /**
   * Set to true when it is currently executing the propagate method
   */
  var executingPropagate = false

  /**
   * True if the constraint is idempotent i.e. calling two times propagate is useless if no other changes occurred
   * sigma(store) = sigma(sigma(store))
   */
  var idempotent = false

  /**
   * @return true if it is currently executing the propagate method.
   */
  def inPropagate() = executingPropagate

  /**
   * @param b
   * @return a garded version of this constraint i.e. that will only be posted when b is true
   */
  def when(b: CPVarBool): Constraint = new Garded(b, this, true)

  /**
   * @param b
   * @return a garded version of this constraint i.e. that will only be posted when b is false
   */
  def whenNot(b: CPVarBool) = new Garded(b, this, false)

  override def toString = "constraint:" + name

  /**
   * setup the constraint, typically this is the place where
   * - the constraint registers to modifications of the domains of variables in its scope
   * - a first consistency check and propagation is done
   * @param l
   * @return The outcome of the first propagation and consistency check
   */
  def setup(l: CPPropagStrength): CPOutcome

  /**
   *
   * @param set the L2 priority (propagate method) in the propagation queue, a number between 0 and CPStore.MAXPRIORL2
   */
  def priorityL2_=(priority: Int) {
    priorL2 = priority;
  }

  private def checkL1Prior(priority: Int) = 0 max (priority min CPStore.MAXPRIORL1)

  def priorityBindL1_=(priority: Int) {
    priorBindL1 = checkL1Prior(priority)
  }

  def priorityRemoveL1_=(priority: Int) {
    priorRemoveL1 = checkL1Prior(priority)
  }

  def priorityBoundsL1_=(priority: Int) {
    priorBoundsL1 = checkL1Prior(priority)
  }

  def priorityRequireL1_=(priority: Int) {
    priorRequireL1 = checkL1Prior(priority)
  }

  def priorityExcludeL1_=(priority: Int) {
    priorExcludeL1 = checkL1Prior(priority)
  }

  def priorityL2 = priorL2

  def priorityBindL1 = priorBindL1

  def priorityRemoveL1 = priorRemoveL1

  def priorityBoundsL1 = priorBoundsL1

  def priorityRequireL1 = priorRequireL1

  def priorityExcludeL1 = priorExcludeL1

  /**
   * @return true if the constraint is still active
   */
  def isActive() = active.value

  /**
   * @return true if the constraint is still in the propagation queue, false otherwise
   */
  def isInQueue() = inQueue.value

  /**
   * Disable the constraint such that it is not propagated any more (will not enter into the propagation queue).
   * Note that this state is reversible (trailable).
   */
  def deactivate() {
    active.value = false
  }

  /**
   * Reactivate the constraint
   */
  def activate() {
    active.value = true
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
  def propagate(): CPOutcome = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callUpdateBoundsIdxWhenBoundsChange(this,idx)
   * @param x has a new minimum and/or maximum value in its domain since last call
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def updateBounds(x: CPVarInt) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callUpdateBoundsIdxWhenBoundsChange(this,idx)
   * @param x has a new minimum and/or maximum value in its domain since last call
   * @param idx is a key value that was given to callUpdateMaxIdxWhenMaxChanges(x,this,idx) attached to variable x.
   *        This is typically used to retrieve the index of x in an array of variables in constant time
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def updateBoundsIdx(x: CPVarInt, idx: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked so
   * with the method call x.callUpdateMaxWhenMaxChanges(this)
   * @param value is the old maximum of x that has changed
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def updateMax(x: CPVarInt, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callUpdateMinIdxWhenMaxChanges(this,idx)
   * @param value is the old maximum of x that has changed
   * @param idx is a key value that was given to callUpdateMaxIdxWhenMaxChanges(x,this,idx) attached to variable x.
   *        This is typically used to retrieve the index of x in an array of variables in constant time
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def updateMaxIdx(x: CPVarInt, idx: Int, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked so
   * with the method call x.callUpdateMinWhenMinChanges(this)
   * @param val is the old minimum of x that has changed
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def updateMin(x: CPVarInt, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callUpdateMinIdxWhenMinChanges(this,idx)
   * @param value is the old maximum of x that has changed
   * @param idx is a key value that was given to callUpdateMaxIdxWhenMinChanges(x,this,idx) attached to variable x.
   *        This is typically used to retrieve the index of x in an array of variables in constant time
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def updateMinIdx(x: CPVarInt, idx: Int, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callValBind(this)
   * @param x is bind
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def valBind(x: CPVarInt) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callValBindIdx(this,idx)
   * @param x is bind
   * @param idx is a key value that was given to callValBindIdx(x,idx) attached to variable x.
   *        This is typically used to retrieve the index of x in an array of variables in constant time
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def valBindIdx(x: CPVarInt, idx: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callValRemoveWhenValueRemoved(this)
   * @param value is a value that has been removed from the domain of x since last call
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def valRemove(x: CPVarInt, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callValRemoveIdxWhenValueRemoved(this)
   * @param value is a value that has been removed from the domain of x since last call
   * @param idx is a key value that was given to callValBind(x,idx) attached to variable x.
   *        This is typically used to retrieve the index of x in an array of variables in constant time
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def valRemoveIdx(x: CPVarInt, idx: Int, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callValRequiredWhenValueRequired(this)
   * @param val is a value that has been put as required in the domain of x since last call
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def valRequired(x: CPVarSet, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callValRequiredIdxWhenValueRemovedIdx(this)
   * @param val is a value that has been put as required in the domain of x since last call
   * @param idx is a key value that was given to callValRequiredIdxWhenValueRemovedIdx(x,idx) attached to variable x.
   *        This is typically used to retrieve the index of x in an array of variables in constant time
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def valRequiredIdx(x: CPVarSet, idx: Int, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callValExcludeddWhenValueRequired(this)
   * @param val is a value that has been excluded in the domain of x since last call
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def valExcluded(x: CPVarSet, value: Int) = CPOutcome.Suspend

  /**
   * Propagation method of Level L1 that is called if variable x has asked to do so
   * with the method call x.callValExcludedIdxWhenValueRemovedIdx(this)
   * @param val is a value that has been put as required in the domain of x since last call
   * @param idx is a key value that was given to callValExcludedIdxWhenValueRemovedIdx(x,idx) attached to variable x.
   *        This is typically used to retrieve the index of x in an array of variables in constant time
   * @return the outcome i.e. Failure, Success or Suspend
   */
  def valExcludedIdx(x: CPVarSet, idx: Int, value: Int) = CPOutcome.Suspend

  def execute(): CPOutcome = {
    inQueue.value = false
    executingPropagate = true
    val oc = propagate()
    if (oc != CPOutcome.Failure) {
      snapshotVarInt()
      snapshotVarSet()
    }
    executingPropagate = false
    if (oc == CPOutcome.Success) {
      deactivate()
    }
    oc
  }

  def setInQueue() {
    inQueue.value = true
  }

}


abstract class DeltaVarInt(x: CPVarInt,filter: DeltaVarInt => CPOutcome) extends Constraint(x.s, "DeltaVarInt") {
  
  val sn = new SnapshotVarInt(x)
  s.onPop {
    sn.update()
  }
  
  override def snapshotVarInt() {
    super.snapshotVarInt()
    sn.update()
  }
  
  override def propagate() = filter(this)
  
  def changed() = x.changed(sn)
  def size() = x.deltaSize(sn)
  def values() = x.delta(sn.oldMin,sn.oldMax,sn.oldSize)
  def minChanged() = x.minChanged(sn)
  def maxChanged() = x.maxChanged(sn)
  def oldMin() = x.oldMin(sn)
  def oldMax() = x.oldMax(sn)
  
}

abstract class DeltaVarSet(x: CPVarSet,filter: DeltaVarSet => CPOutcome) extends Constraint(x.store, "DeltaVarSet") {
  
  val sn = new SnapshotVarSet(x)
  s.onPop {
    sn.update()
  }
  
  override def snapshotVarSet() {
    super.snapshotVarSet()
    sn.update()
  }
  
  override def propagate() = filter(this)
  
  def changed() = x.changed(sn)
  def possibleChanged() = x.possibleChanged(sn)
  def requiredChanged() = x.requiredChanged(sn)
  def deltaPossibleSize() = x.deltaPossibleSize(sn)
  def deltaRequiredSize() = x.deltaRequiredSize(sn)
  def deltaPossible(): Iterator[Int] = x.deltaPossible(sn)
  def deltaRequired(): Iterator[Int] = x.deltaRequired(sn)
  
}
