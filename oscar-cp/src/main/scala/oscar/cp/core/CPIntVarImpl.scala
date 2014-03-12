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

import oscar.algo.reversible.ReversibleQueue
import oscar.algo.reversible.ReversiblePointer
import scala.collection._
import scala.collection.generic._
import oscar.cp.core.domains.IntDomain
import oscar.cp.core.domains.AdaptableIntDomain

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPIntVarImpl(st: CPStore, minimum: Int, maximum: Int, name: String = "") extends CPIntVar(st, name) {

  // Reversible pointer to the domain structure (can change from interval to sparse set) 
  private val domain: ReversiblePointer[IntDomain] = AdaptableIntDomain(st, minimum, maximum)
  
  val onBoundsL2 = new ReversiblePointer[ConstraintQueue](store, null)
  val onBindL2 = new ReversiblePointer[ConstraintQueue](store, null)
  val onDomainL2 = new ReversiblePointer[ConstraintQueue](store, null)

  val onBoundsL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  val onBindL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  val onDomainL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)

  val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  val onBindIdxL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)
  val onDomainIdxL1 = new ReversiblePointer[PropagEventQueueVarInt](store, null)

  /**
   * Builds a variable with domain defined by the range into the store s
   * @param r a scala range
   */
  def this(st: CPStore, r: Range) = this(st, r.start, if (r.isInclusive) r.end else r.end - 1)

  def transform(v: Int ) = v

  def iterator: Iterator[Int] = domain.value.iterator

  /**
   *
   * @return The number of propagation methods of L2 attached to changes of this variables.
   */
  def constraintDegree() = {
    var tot = 0
    if (onBoundsL2.hasValue()) tot += onBoundsL2.value.size
    if (onBindL2.hasValue()) tot += onBindL2.value.size
    if (onDomainL2.hasValue()) tot += onDomainL2.value.size

    if (onBoundsL1.hasValue()) tot += onBoundsL1.value.size
    if (onBindL1.hasValue()) tot += onBindL1.value.size
    if (onDomainL1.hasValue()) tot += onDomainL1.value.size

    if (onBoundsIdxL1.hasValue()) tot += onBoundsIdxL1.value.size
    if (onBindIdxL1.hasValue()) tot += onBindIdxL1.value.size
    if (onDomainIdxL1.hasValue()) tot += onDomainIdxL1.value.size
    tot
  }

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean = {
    assert(!store.isFailed())
    size == 1
  }

  /**
   *
   * @param v
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  def isBoundTo(v: Int): Boolean = isBound && value == v

  /**
   * Test if a value is in the domain
   * @param val
   * @return  true if the domain contains the value val, false otherwise
   */
  def hasValue(value: Int) = domain.value.hasValue(value)

  /**
   * @param val
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  def valueAfter(value: Int): Int = {
    if (max < value + 1) {
      println("error: no value after " + value + " maximum=" + max)
      value
    } else {
      domain.value.nextValue(value + 1)
    }
  }

  /**
   * @param val
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  def valueBefore(value: Int): Int = {
    if (min > value - 1) {
      println("error: no value before " + value + " minimum=" + min)
      value
    } else {
      domain.value.prevValue(value - 1)
    }
  }

  /**
   * @return  the size of the domain
   */
  override def size = domain.value.size

  /**
   * @return true if the domain is empty, false otherwise
   */
  override def isEmpty = domain.value.isEmpty

  /**
   * @return  the minimum value in the domain
   */
  def min = {
    assert(!domain.value.isEmpty)
    domain.value.min
  }

  /**
   * @return  the maximum value in the domain
   */
  def max = {
    assert(!domain.value.isEmpty)
    domain.value.max
  }

  override def toString(): String = {
    if (isBound) {
      if (name.isEmpty()) value.toString
      else name + " " + value
    }
    else {
      if (name.isEmpty()) domain.value.toString
      else name + " " + domain.value
    }
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBind(c: Constraint, trackDelta: Boolean = false) {
    onBindL2.setValue(new ConstraintQueue(onBindL2.value, c))
    if (trackDelta) c.addSnapshot(this)
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the maximum or the minimum value of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBoundsChange(c: Constraint, trackDelta: Boolean = false) {
    onBoundsL2.setValue(new ConstraintQueue(onBoundsL2.value, c))
    if (trackDelta) c.addSnapshot(this)
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * one of the value is removed from the domain
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenDomainChanges(c: Constraint, trackDelta: Boolean = false) {
    onDomainL2.setValue(new ConstraintQueue(onDomainL2.value, c))
    if (trackDelta) c.addSnapshot(this)
  }

  /**
   * Level 1 registration: ask that the valBind(CPIntVar) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#valBind(CPIntVar)
   */
  def callValBindWhenBind(c: Constraint) {
    callValBindWhenBind(c, this)
  }

  def callValBindWhenBind(c: Constraint, variable: CPIntVar) {
    onBindL1.setValue(new PropagEventQueueVarInt(onBindL1.value, c, variable))
  }

  /**
   * Level 1 registration: ask that the updateBounds(CPIntVar) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateBounds(CPIntVar)
   */
  def callUpdateBoundsWhenBoundsChange(c: Constraint) {
    callUpdateBoundsWhenBoundsChange(c, this)
  }

  def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntVar) {
    onBoundsL1.setValue(new PropagEventQueueVarInt(onBoundsL1.value, c, variable))
  }

  /**
   * Level 1 registration: ask that the valRemove(CPIntVar, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @see oscar.cp.core.Constraint#valRemove(CPIntVar, int)
   */
  def callValRemoveWhenValueIsRemoved(c: Constraint) {
    callValRemoveWhenValueIsRemoved(c, this)
  }

  def callValRemoveWhenValueIsRemoved(c: Constraint, variable: CPIntVar) {
    onDomainL1.setValue(new PropagEventQueueVarInt(onDomainL1.value, c, variable))
  }

  /**
   * Level 1 registration: ask that the valRemoveIdx(CPIntVar, int, int) method of the constraint c is called for each
   * value deletion from the domain
   * @param c
   * @param idx, an index that will be given as parameter to valRemoveIdx(CPIntVar, int, int)
   * @see Constraint#valRemoveIdx(CPIntVar, int, int)
   */
  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, idx: Int) {
    callValRemoveIdxWhenValueIsRemoved(c, this, idx)
  }

  def callValRemoveIdxWhenValueIsRemoved(c: Constraint, variable: CPIntVar, idx: Int) {
    onDomainIdxL1.setValue(new PropagEventQueueVarInt(onDomainIdxL1.value, c, variable, idx))
  }

  /**
   * Level 1 registration: ask that the updateBoundsIdx(CPIntVar, int) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateBoundsIdx(CPIntVar, int)
   * @see Constraint#updateBoundsIdx(CPIntVar, int)
   */
  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) {
    callUpdateBoundsIdxWhenBoundsChange(c, this, idx)
  }

  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntVar, idx: Int) {
    onBoundsIdxL1.setValue(new PropagEventQueueVarInt(onBoundsIdxL1.value, c, variable, idx))
  }

  /**
   * Level 1 registration: ask that the valBindIdx(CPIntVar, int) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @param idx, an index that will be given as parameter to valBindIdx(CPIntVar, int)
   * @see Constraint#valBindIdx(CPIntVar, int)
   */
  def callValBindIdxWhenBind(c: Constraint, idx: Int) {
    callValBindIdxWhenBind(c, this, idx)
  }

  def callValBindIdxWhenBind(c: Constraint, variable: CPIntVar, idx: Int) {
    onBindIdxL1.setValue(new PropagEventQueueVarInt(onBindIdxL1.value, c, variable, idx))
  }

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  def assign(value: Int): CPOutcome = {
    if (!hasValue(value)) return CPOutcome.Failure
    else if (isBound) return CPOutcome.Suspend
    else { // more than one value
      val assignToMax = max == value
      val assignToMin = min == value
      // -------- AC3 notifications ------------
      //println(this+" notifyL2 onBounds "+onBoundsL2.value)
      store.notifyL2(onBoundsL2.value)
      store.notifyL2(onDomainL2.value)
      store.notifyL2(onBindL2.value)
      // --------- AC5 notifications ------------
      store.notifyBindL1(onBindL1.value, this)
      store.notifyBindIdxL1(onBindIdxL1.value, this)
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      // must notify AC5 event before the actual removal
      if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
        var i = domain.value.min
        while (i <= domain.value.max) {
          if (i != value && domain.value.hasValue(i)) {
            if (onDomainL1.hasValue()) {
              store.notifRemoveL1(onDomainL1.value, this, i)
            }
            if (onDomainIdxL1.hasValue()) {
              store.notifyRemoveIdxL1(onDomainIdxL1.value, this, i)
            }
          }
          i += 1
        }
      }
      // finally do the assignment
      return domain.value.assign(value)
    }
  }

  /**
   * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
   */
  def updateMin(value: Int): CPOutcome = {
    
    val dom = domain.value

    if (value > dom.max) return CPOutcome.Failure
    
    val omin = dom.min
    
    if (value <= omin) return CPOutcome.Suspend

    //must notif AC5 event with the removed values before the actual removal
    if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
      var i = omin
      while (i < value) {
        if (dom.hasValue(i)) {
          if (onDomainL1.hasValue())
            store.notifRemoveL1(onDomainL1.value, this, i)
          if (onDomainIdxL1.hasValue())
            store.notifyRemoveIdxL1(onDomainIdxL1.value, this, i)
        }
        i += 1
      }
    }

    val ok = dom.updateMin(value)
    assert(ok != CPOutcome.Failure)

    if (dom.size == 1) {
      assert(isBound)
      store.notifyBindL1(onBindL1.value, this)
      store.notifyBindIdxL1(onBindIdxL1.value, this)
      store.notifyL2(onBindL2.value)
    }
    store.notifyUpdateBoundsL1(onBoundsL1.value, this)
    store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
    store.notifyL2(onBoundsL2.value)
    store.notifyL2(onDomainL2.value)
    return CPOutcome.Suspend
  }

  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
   */
  def updateMax(value: Int): CPOutcome = {
    
    val dom = domain.value
    
    if (value < dom.min) return CPOutcome.Failure
     
    val omax = dom.max
    
    if (value >= omax) return CPOutcome.Suspend

    //must notifyAC3 the removed value before the actual removal
    if (onDomainL1.hasValue() || onDomainIdxL1.hasValue()) {
      var i = omax
      while (i > value) {
        if (dom.hasValue(i)) {
          if (onDomainL1.hasValue())
            store.notifRemoveL1(onDomainL1.value, this, i)
          if (onDomainIdxL1.hasValue())
            store.notifyRemoveIdxL1(onDomainIdxL1.value, this, i)
        }
        i -= 1
      }
    }

    val ok = dom.updateMax(value)
    assert(ok != CPOutcome.Failure)

    if (dom.size == 1) {
      assert(isBound)
      store.notifyBindL1(onBindL1.value, this)
      store.notifyBindIdxL1(onBindIdxL1.value, this)
      store.notifyL2(onBindL2.value)
    }
    store.notifyUpdateBoundsL1(onBoundsL1.value, this)
    store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
    store.notifyL2(onBoundsL2.value)
    store.notifyL2(onDomainL2.value)
    return CPOutcome.Suspend
  }

  /**
   * Remove val from the domain, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if the domain is not equal to the singleton {val}, Failure otherwise
   */
  def removeValue(value: Int): CPOutcome = {
    
    val dom = domain.value
    
    val omin = dom.min
    val omax = dom.max
    val minRemoved = dom.min == value
    val maxRemoved = dom.max == value
    val indom = dom.hasValue(value)

    if (!indom) return CPOutcome.Suspend

    val ok = dom.removeValue(value)
    if (ok == CPOutcome.Failure) return CPOutcome.Failure

    if (minRemoved || maxRemoved) {
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      store.notifyL2(onBoundsL2.value)
    }
    if (indom) {
      store.notifRemoveL1(onDomainL1.value, this, value)
      store.notifyRemoveIdxL1(onDomainIdxL1.value, this, value)
      store.notifyL2(onDomainL2.value)
    }
    if (isBound) {
      store.notifyBindL1(onBindL1.value, this)
      store.notifyBindIdxL1(onBindIdxL1.value, this)
      store.notifyL2(onBindL2.value)
    }
    return ok
  }

  // ----------------------------------

  def delta(oldMin: Int, oldMax: Int, oldSize: Int): Iterator[Int] = {
    domain.value.delta(oldMin, oldMax, oldSize)
  }

  def changed(c: Constraint): Boolean = changed(c.snapshotsVarInt(this))

  def minChanged(c: Constraint): Boolean = minChanged(c.snapshotsVarInt(this))

  def maxChanged(c: Constraint): Boolean = maxChanged(c.snapshotsVarInt(this))

  def boundsChanged(c: Constraint): Boolean = boundsChanged(c.snapshotsVarInt(this))

  def oldMin(c: Constraint): Int = oldMin(c.snapshotsVarInt(this))

  def oldMax(c: Constraint): Int = oldMax(c.snapshotsVarInt(this))

  def oldSize(c: Constraint): Int = oldSize(c.snapshotsVarInt(this))

  def deltaSize(c: Constraint): Int = deltaSize(c.snapshotsVarInt(this))

  def delta(c: Constraint): Iterator[Int] = {
    val sn = c.snapshotsVarInt(this)
    delta(sn.oldMin, sn.oldMax, sn.oldSize)
  }
}
