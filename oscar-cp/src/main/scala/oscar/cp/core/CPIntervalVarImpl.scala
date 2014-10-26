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
import scala.util.Random
import oscar.cp.core.domains.IntDomain
import oscar.cp.core.domains.AdaptableIntDomain
import oscar.cp.core.CPOutcome._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPIntervalVarImpl(store: CPStore, private val domain: IntDomain, name: String = "") extends CPIntervalVar(store, name) {

  val onBoundsL2 = new ReversiblePointer[ConstraintQueue](store, null)
  val onBindL2 = new ReversiblePointer[ConstraintQueue](store, null)

  val onBoundsL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  val onBindL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)

  val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  val onBindIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)

  def transform(v: Int) = v

  def iterator: Iterator[Int] = domain.iterator

  /**
   *
   * @return The number of propagation methods of L2 attached to changes of this variables.
   */
  def constraintDegree() = {
    var tot = 0
    if (onBoundsL2.hasValue()) tot += onBoundsL2.value.size
    if (onBindL2.hasValue()) tot += onBindL2.value.size

    if (onBoundsL1.hasValue()) tot += onBoundsL1.value.size
    if (onBindL1.hasValue()) tot += onBindL1.value.size

    if (onBoundsIdxL1.hasValue()) tot += onBoundsIdxL1.value.size
    if (onBindIdxL1.hasValue()) tot += onBindIdxL1.value.size
    tot
  }

  /**
   * @return true if the domain of the variable has exactly one value, false if the domain has more than one value
   */
  def isBound: Boolean = {
    assert(!store.isFailed())
    domain.isBound
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
  def hasValue(value: Int) = domain.hasValue(value)

  /**
   * @param val
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  def valueAfter(value: Int): Int = {
    if (max < value + 1) {
      println("error: no value after " + value + " maximum=" + max)
      value
    } else {
      domain.nextValue(value + 1)
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
      domain.prevValue(value - 1)
    }
  }

  /**
   * @return A random value in the domain of the variable (uniform distribution)
   */
  override def randomValue(rand: Random): Int = domain.randomValue(rand)

  /**
   * @return  the size of the domain
   */
  override def size = domain.size

  /**
   * @return true if the domain is empty, false otherwise
   */
  override def isEmpty = domain.isEmpty

  /**
   * @return  the minimum value in the domain
   */
  def min = {
    assert(!domain.isEmpty)
    domain.min
  }

  /**
   * @return  the maximum value in the domain
   */
  def max = {
    assert(!domain.isEmpty)
    domain.max
  }

  override def toString(): String = {
    if (isBound) {
      if (name.isEmpty()) value.toString
      else name + " " + value
    } else {
      if (name.isEmpty()) domain.toString
      else name + " " + domain.toString
    }
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBind(c: Constraint) {
    onBindL2.setValue(new ConstraintQueue(onBindL2.value, c))
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the maximum or the minimum value of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBoundsChange(c: Constraint) {
    onBoundsL2.setValue(new ConstraintQueue(onBoundsL2.value, c))
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

  def callValBindWhenBind(c: Constraint, variable: CPIntervalVar) {
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

  def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar) {
    onBoundsL1.setValue(new PropagEventQueueVarInt(onBoundsL1.value, c, variable))
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

  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int) {
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

  def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int) {
    onBindIdxL1.setValue(new PropagEventQueueVarInt(onBindIdxL1.value, c, variable, idx))
  }

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  def assign(value: Int): CPOutcome = {
    val dom = domain
    if (!dom.hasValue(value)) CPOutcome.Failure
    else if (dom.isBound) CPOutcome.Suspend
    else { // more than one value
      val assignToMax = max == value
      val assignToMin = min == value
      // -------- AC3 notifications ------------
      //println(this+" notifyL2 onBounds "+onBoundsL2.value)
      store.notifyL2(onBoundsL2.value)
      store.notifyL2(onBindL2.value)
      // --------- AC5 notifications ------------
      store.notifyBindL1(onBindL1.value, this)
      store.notifyBindIdxL1(onBindIdxL1.value, this)
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      // finally do the assignment
      dom.assign(value)
    }
  }

  /**
   * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
   */
  def updateMin(value: Int): CPOutcome = {

    val dom = domain

    if (value > dom.max) return CPOutcome.Failure

    val omin = dom.min

    if (value <= omin) return CPOutcome.Suspend

    val ok = dom.updateMin(value)
    assert(ok != CPOutcome.Failure)

    if (dom.isBound) {
      store.notifyBindL1(onBindL1.value, this)
      store.notifyBindIdxL1(onBindIdxL1.value, this)
      store.notifyL2(onBindL2.value)
    }
    store.notifyUpdateBoundsL1(onBoundsL1.value, this)
    store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
    store.notifyL2(onBoundsL2.value)
    return CPOutcome.Suspend
  }

  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
   */
  def updateMax(value: Int): CPOutcome = {

    val dom = domain

    if (value < dom.min) return CPOutcome.Failure

    val omax = dom.max

    if (value >= omax) return CPOutcome.Suspend


    val ok = dom.updateMax(value)

    if (dom.isBound) {
      store.notifyBindL1(onBindL1.value, this)
      store.notifyBindIdxL1(onBindIdxL1.value, this)
      store.notifyL2(onBindL2.value)
    }
    store.notifyUpdateBoundsL1(onBoundsL1.value, this)
    store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
    store.notifyL2(onBoundsL2.value)
    return CPOutcome.Suspend
  }
  
  

}

object CPIntervalVarImpl {
  def apply(store: CPStore, minimum: Int, maximum: Int, name: String = ""): CPIntervalVarImpl = {
    val domain = new AdaptableIntDomain(store, minimum, maximum)
    new CPIntervalVarImpl(store, domain, name)
  }
}