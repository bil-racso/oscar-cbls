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
 * *****************************************************************************/

package oscar.cp.core

import oscar.algo.reversible.ReversiblePointer
import oscar.algo.reversible.ReversibleInt
import oscar.cp.core.CPOutcome._
import scala.util.Random

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */
class CPIntervalVarImpl(store: CPStore, initialMin: Int, initialMax: Int, name: String) extends CPIntervalVar(store, name) {

  // Adjacency list
  private final val onBoundsL2 = new ReversiblePointer[ConstraintQueue](store, null)
  private final val onBindL2 = new ReversiblePointer[ConstraintQueue](store, null)
  private final val onBoundsL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private final val onBindL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private final val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private final val onBindIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)

  // Domain representation
  private final val revMin = new ReversibleInt(store, initialMin)
  private final val revMax = new ReversibleInt(store, initialMax)

  final def transform(v: Int) = v

  final def iterator: Iterator[Int] = new Iterator[Int] {
    var i = revMin.value - 1
    val n = revMax.value
    override def hasNext: Boolean = i < n
    override def next(): Int = {
      i += 1
      i
    }
  }

  /** @return The number of propagation methods of L2 attached to changes of this variables. */
  final def constraintDegree: Int = {
    var tot = 0
    if (onBoundsL2.hasValue) tot += onBoundsL2.value.size
    if (onBindL2.hasValue) tot += onBindL2.value.size
    if (onBoundsL1.hasValue) tot += onBoundsL1.value.size
    if (onBindL1.hasValue) tot += onBindL1.value.size
    if (onBoundsIdxL1.hasValue) tot += onBoundsIdxL1.value.size
    if (onBindIdxL1.hasValue) tot += onBindIdxL1.value.size
    tot
  }

  /** @return true if the domain of the variable has exactly one value, false if the domain has more than one value */
  @inline final def isBound: Boolean = {
    assert(!store.isFailed())
    revMax.value == revMin.value
  }

  /**
   * @param v
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  @inline final def isBoundTo(v: Int): Boolean = revMax.value == v && revMin.value == v

  /**
   * Test if a value is in the domain
   * @param val
   * @return  true if the domain contains the value val, false otherwise
   */
  @inline final def hasValue(value: Int): Boolean = revMin.value <= value && value <= revMax.value

  /**
   * @param val
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  final def valueAfter(value: Int): Int = {
    val max = revMax.value
    val v = value + 1
    if (max < v) {
      println("error: no value after " + value + " maximum=" + max)
      value
    } else {
      val min = revMin.value
      if (isEmpty || v > max) value
      else if (v < min) min
      else v
    }
  }

  /**
   * @param val
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  final def valueBefore(value: Int): Int = {
    val min = revMin.value
    val v = value - 1
    if (min > v) {
      println("error: no value before " + value + " minimum=" + min)
      value
    } else {
      val max = revMax.value
      if (isEmpty || v < min) value
      else if (v > max) max
      else v
    }
  }

  /** @return A random value in the domain of the variable (uniform distribution) */
  final override def randomValue(rand: Random): Int = {
    assert(!isEmpty)
    val min = revMin.value
    val max = revMax.value
    min + rand.nextInt(max - min + 1)
  }

  /**
   * @return  the size of the domain
   */
  @inline final override def size = revMax.value - revMin.value + 1

  /**
   * @return true if the domain is empty, false otherwise
   */
  @inline final override def isEmpty = revMax.value < revMin.value

  /**
   * @return  the minimum value in the domain
   */
  @inline final override def min = {
    assert(!isEmpty)
    revMin.value
  }

  /**
   * @return  the maximum value in the domain
   */
  @inline final override def max = {
    assert(!isEmpty)
    revMax.value
  }

  final override def toString: String = {
    if (isEmpty) "phi"
    else if (isBound) {
      val min = revMin.value
      if (name.isEmpty) min.toString
      else name + " " + min
    } else {
      val min = revMin.value
      val max = revMax.value
      if (name.isEmpty) s"$name [$min, $max]"
      else s"[$min, $max]"
    }
  }

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  @inline final def assign(value: Int): CPOutcome = {
    val min = revMin.value
    val max = revMax.value
    if (value < min || max < value) throw Inconsistency
    else if (min == max) Suspend
    else {
      revMin.value = value
      revMax.value = value
      // Notify constraints
      store.notifyL2(onBindL2.value)
      store.notifyL2(onBoundsL2.value)
      store.notifyBindL1(onBindL1.value, this)
      store.notifyBindIdxL1(onBindIdxL1.value, this)
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      Suspend
    }
  }

  /**
   * Remove from the domain all values < val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value >= val in the domain, Failure otherwise
   */
  @inline final def updateMin(value: Int): CPOutcome = {
    val max = revMax.value
    if (value > max) throw Inconsistency
    else {
      val oldMin = revMin.value
      if (value <= oldMin) Suspend
      else {
        revMin.value = value
        // Notify the constraints
        store.notifyUpdateBoundsL1(onBoundsL1.value, this)
        store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
        store.notifyL2(onBoundsL2.value)
        if (max == value) { // is bound
          store.notifyBindL1(onBindL1.value, this)
          store.notifyBindIdxL1(onBindIdxL1.value, this)
          store.notifyL2(onBindL2.value)
        }
        Suspend
      }
    }
  }

  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
   */
  @inline final def updateMax(value: Int): CPOutcome = {
    val min = revMin.value
    if (value < min) throw Inconsistency
    else {
      val oldMax = revMax.value
      if (value >= oldMax) Suspend
      else {
        revMax.value = value
        // Notify the constraints
        store.notifyUpdateBoundsL1(onBoundsL1.value, this)
        store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
        store.notifyL2(onBoundsL2.value)
        if (value == min) { // is bound
          store.notifyBindL1(onBindL1.value, this)
          store.notifyBindIdxL1(onBindIdxL1.value, this)
          store.notifyL2(onBindL2.value)
        }
        Suspend
      }
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
}

object CPIntervalVarImpl {
  def apply(store: CPStore, minimum: Int, maximum: Int, name: String = ""): CPIntervalVarImpl = {
    new CPIntervalVarImpl(store, minimum, maximum, name)
  }
}