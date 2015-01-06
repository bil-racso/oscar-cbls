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
import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPOutcome._
import scala.util.Random
import oscar.algo.reversible.ReversibleInt

/**
 * @author Pierre Schaus pschaus@gmail.com
 * @author Renaud Hartert ren.hartert@gmail.com
 */

class IntervalVarTrailEntry(variable: CPIntervalVarImpl, min: Int, max: Int) extends TrailEntry {
  @inline final override def restore(): Unit = {
    variable.min = min
    variable.max = max
  }
}

class CPIntervalVarImpl(final override val store: CPStore, initialMin: Int, initialMax: Int, final override val name: String) extends CPIntervalVar {

  private[this] var lastMagic: Long = -1L

  @inline final protected def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      store.trail(new IntervalVarTrailEntry(this, min, max))
    }
  }

  // Registered constraints
  private[this] val onBoundsL2 = new ReversiblePointer[ConstraintQueue](store, null)
  private[this] val onBindL2 = new ReversiblePointer[ConstraintQueue](store, null)
  private[this] val onBoundsL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onBindL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onBoundsIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  private[this] val onBindIdxL1 = new ReversiblePointer[PropagEventQueueVarInt[CPIntervalVar]](store, null)
  
  // Number of constraints registered on the variable
  private[this] val degree = new ReversibleInt(store, 0) // should not change often

  // Domain representation
  private[this] var _min: Int = initialMin // private[this] is used for more efficient bytecode
  private[this] var _max: Int = initialMax // private[this] is used for more efficient bytecode
  
  @inline final override def min: Int = _min
  
  @inline final override def max: Int = _max
  
  @inline final def min_=(newMin: Int): Unit = _min = newMin
  
  @inline final def max_=(newMax: Int): Unit = _max = newMax

  final def transform(v: Int) = v

  final def iterator: Iterator[Int] = new Iterator[Int] {
    var i = _min - 1
    val n = _max
    override def hasNext: Boolean = i < n
    override def next(): Int = {
      i += 1
      i
    }
  }

  /** @return The number of propagators registered on this variables. */
  final def constraintDegree: Int = degree.value

  /** @return true if the domain of the variable has exactly one value, false if the domain has more than one value */
  @inline final def isBound: Boolean = {
    assert(!store.isFailed())
    _max == _min
  }

  /**
   * @param v
   * @return true if the variable is bound to value v, false if variable is not bound or bound to another value than v
   */
  @inline final def isBoundTo(v: Int): Boolean = _max == v && _min == v

  /**
   * Test if a value is in the domain
   * @param val
   * @return  true if the domain contains the value val, false otherwise
   */
  @inline final def hasValue(value: Int): Boolean = _min <= value && value <= _max

  /**
   * @param val
   * @return the smallest value > val in the domain, None if there is not value > val in the domain
   */
  final def valueAfter(value: Int): Int = {
    val v = value + 1
    if (_max < v) {
      println("error: no value after " + value + " maximum=" + _max)
      value
    } else if (isEmpty || v > _max) value
    else if (v < _min) _min
    else v
  }

  /**
   * @param val
   * @return the largest value < val in the domain, None if there is not value < val in the domain
   */
  final def valueBefore(value: Int): Int = {
    val v = value - 1
    if (_min > v) {
      println("error: no value before " + value + " minimum=" + _min)
      value
    } else if (isEmpty || v < _min) value
    else if (v > _max) _max
    else v

  }

  /** @return A random value in the domain of the variable (uniform distribution) */
  final override def randomValue(rand: Random): Int = {
    assert(!isEmpty)
    _min + rand.nextInt(_max - _min + 1)
  }

  /**
   * @return  the size of the domain
   */
  @inline final override def size: Int = _max - _min + 1

  /**
   * @return true if the domain is empty, false otherwise
   */
  @inline final override def isEmpty: Boolean = _max < _min

  final override def toString: String = {
    if (isEmpty) "phi"
    else if (isBound) {
      if (name.isEmpty) _min.toString
      else name + " " + _min
    } else {
      if (name.isEmpty) s"$name [${_min}, ${_max}]"
      else s"[${_min}, ${_max}]"
    }
  }

  /**
   * Reduce the domain to the singleton {val}, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if val was in the domain, Failure otherwise
   */
  @inline final def assign(value: Int): CPOutcome = {
    if (value < _min || _max < value) throw Inconsistency
    else if (_min == _max) Suspend
    else {
      trail()
      _min = value
      _max = value
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
    if (value > _max) throw Inconsistency
    else if (value <= _min) Suspend
    else {
      trail()
      _min = value
      // Notify the constraints
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      store.notifyL2(onBoundsL2.value)
      if (_max == value) { // is bound
        store.notifyBindL1(onBindL1.value, this)
        store.notifyBindIdxL1(onBindIdxL1.value, this)
        store.notifyL2(onBindL2.value)
      }
      Suspend
    }
  }

  /**
   * Remove from the domain all values > val, and notify appropriately all the propagators registered to this variable
   * @param val
   * @return  Suspend if there is at least one value <= val in the domain, Failure otherwise
   */
  @inline final def updateMax(value: Int): CPOutcome = {
    if (value < _min) throw Inconsistency
    else if (value >= _max) Suspend
    else {
      trail()
      _max = value
      // Notify the constraints
      store.notifyUpdateBoundsL1(onBoundsL1.value, this)
      store.notifyUpdateBoundsIdxL1(onBoundsIdxL1.value, this)
      store.notifyL2(onBoundsL2.value)
      if (value == _min) { // is bound
        store.notifyBindL1(onBindL1.value, this)
        store.notifyBindIdxL1(onBindIdxL1.value, this)
        store.notifyL2(onBindL2.value)
      }
      Suspend
    }
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBind(c: Constraint): Unit = {
    degree.incr()
    onBindL2.setValue(new ConstraintQueue(onBindL2.value, c))
  }

  /**
   * Level 2 registration: ask that the propagate() method of the constraint c is called whenever
   * the maximum or the minimum value of the domain changes
   * @param c
   * @see oscar.cp.core.Constraint#propagate()
   */
  def callPropagateWhenBoundsChange(c: Constraint): Unit = {
    degree.incr()
    onBoundsL2.setValue(new ConstraintQueue(onBoundsL2.value, c))
  }

  /**
   * Level 1 registration: ask that the valBind(CPIntVar) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @see oscar.cp.core.Constraint#valBind(CPIntVar)
   */
  def callValBindWhenBind(c: Constraint): Unit = {
    callValBindWhenBind(c, this)
  }

  def callValBindWhenBind(c: Constraint, variable: CPIntervalVar): Unit = {
    degree.incr()
    onBindL1.setValue(new PropagEventQueueVarInt(onBindL1.value, c, variable))
  }

  /**
   * Level 1 registration: ask that the updateBounds(CPIntVar) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes.
   * @param c
   * @see oscar.cp.core.Constraint#updateBounds(CPIntVar)
   */
  def callUpdateBoundsWhenBoundsChange(c: Constraint): Unit = {
    callUpdateBoundsWhenBoundsChange(c, this)
  }

  def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar): Unit = {
    degree.incr()
    onBoundsL1.setValue(new PropagEventQueueVarInt(onBoundsL1.value, c, variable))
  }

  /**
   * Level 1 registration: ask that the updateBoundsIdx(CPIntVar, int) method of the constraint c is called whenever
   * the minimum or maximum value of the domain changes
   * @param c
   * @param idx, an index that will be given as parameter to updateBoundsIdx(CPIntVar, int)
   * @see Constraint#updateBoundsIdx(CPIntVar, int)
   */
  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int): Unit = {
    callUpdateBoundsIdxWhenBoundsChange(c, this, idx)
  }

  def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int): Unit = {
    degree.incr()
    onBoundsIdxL1.setValue(new PropagEventQueueVarInt(onBoundsIdxL1.value, c, variable, idx))
  }

  /**
   * Level 1 registration: ask that the valBindIdx(CPIntVar, int) method of the constraint c is called whenever
   * the domain of the variable is a singleton (i.e. isBound).
   * @param c
   * @param idx, an index that will be given as parameter to valBindIdx(CPIntVar, int)
   * @see Constraint#valBindIdx(CPIntVar, int)
   */
  def callValBindIdxWhenBind(c: Constraint, idx: Int): Unit = {
    callValBindIdxWhenBind(c, this, idx)
  }

  def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int): Unit = {
    degree.incr()
    onBindIdxL1.setValue(new PropagEventQueueVarInt(onBindIdxL1.value, c, variable, idx))
  }
}

object CPIntervalVarImpl {
  def apply(store: CPStore, minimum: Int, maximum: Int, name: String = ""): CPIntervalVarImpl = {
    new CPIntervalVarImpl(store, minimum, maximum, name)
  }
}