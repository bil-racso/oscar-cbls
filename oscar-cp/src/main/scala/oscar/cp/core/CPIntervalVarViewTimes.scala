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

import scala.util.Random

/**
 * Represents a view on variable applying an offset on it.
 * @author Cyrille Dejemeppe Cyrille.Dejemeppe@gmail.com
 * @author Steven Gay steven.gay@uclouvain.be
 */
class CPIntervalVarViewTimes(v: CPIntervalVar, val a: Int) extends CPIntervalVar {
  
  require(a != 0, "a should be different than 0")
  
  final override val store: CPStore = v.store
  
  final override val name: String = s"${v.name} * $a"

  final override def transform(v: Int) = a * this.v.transform(v)

  final override def isBound = v.isBound

  final override def size = v.size

  final override def isEmpty = v.isEmpty

  final override def constraintDegree = v.constraintDegree

  final override def isBoundTo(value: Int): Boolean = if (value % a != 0) false else v.isBoundTo(value / a)
  
  final override def hasValue(value: Int): Boolean = if (value % a != 0) false else v.hasValue(value / a)

  // Scala's division always rounds to the integer closest to zero, but we need flooring/ceiling versions.
  // The following divisions are just a little faster than using the modulo version,
  // and safer+faster than using casting to Double and using Double's ceil/floor 
  @inline
  private def floor_div(a: Int, b: Int) = {
    val q = a / b
    if (a < 0 && q * b != a) q - 1
    else q
  }

  @inline
  private def ceiling_div(a: Int, b: Int) = {
    val q = a / b
    if (a > 0 && q * b != a) q + 1
    else q
  }

  override final def valueAfter(value: Int): Int = v.valueAfter(floor_div(value, a)) * a

  override final def valueBefore(value: Int): Int = v.valueBefore(ceiling_div(value, a)) * a

  override final def randomValue(rand: Random): Int = v.randomValue(rand) * a

  override final def updateMin(value: Int) = v.updateMin(ceiling_div(value, a))

  override final def updateMax(value: Int) = v.updateMax(floor_div(value, a))

  override final def assign(value: Int) = if (value % a == 0) v.assign(value / a) else CPOutcome.Failure

  override final def min = a * v.min

  override final def max = a * v.max

  override final def iterator: Iterator[Int] = v.iterator.map(_ * a)

  final override def toString() = "view with multiplicator " + a + " on (" + v + ")";

  override final def callPropagateWhenBind(c: Constraint) = v.callPropagateWhenBind(c)

  override final def callPropagateWhenBoundsChange(c: Constraint) = v.callPropagateWhenBoundsChange(c)

  // this method is useful when you have a view defined on a view
  override final def callValBindWhenBind(c: Constraint, variable: CPIntervalVar) = v.callValBindWhenBind(c, variable)

  override final def callValBindWhenBind(c: Constraint) = v.callValBindWhenBind(c, this)

  // this method is useful when you have a view defined on a view
  override final def callUpdateBoundsWhenBoundsChange(c: Constraint, variable: CPIntervalVar) = v.callUpdateBoundsWhenBoundsChange(c, variable)

  override final def callUpdateBoundsWhenBoundsChange(c: Constraint) = v.callUpdateBoundsWhenBoundsChange(c, this)

  // this method is useful when you have a view defined on a view
  override final def callValBindIdxWhenBind(c: Constraint, variable: CPIntervalVar, idx: Int) = v.callValBindIdxWhenBind(c, variable, idx)

  override final def callValBindIdxWhenBind(c: Constraint, idx: Int) = v.callValBindIdxWhenBind(c, this, idx)

  // this method is useful when you have a view defined on a view
  override final def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, variable: CPIntervalVar, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, variable, idx);

  override final def callUpdateBoundsIdxWhenBoundsChange(c: Constraint, idx: Int) = v.callUpdateBoundsIdxWhenBoundsChange(c, this, idx)
}
  
