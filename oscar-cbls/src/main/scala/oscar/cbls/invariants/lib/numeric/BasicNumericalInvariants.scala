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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by CETIC www.cetic.be
  *         by Renaud De Landtsheer
  *            Yoann Guyot
  ******************************************************************************/

package oscar.cbls.invariants.lib.numeric

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.logic._;

object Sum{
  def apply(vars: Array[IntValue], cond: SetValue) = SumElements(vars, cond)
  def apply(vars: Iterable[IntValue]) = new Sum(vars)
  def apply(vars:Array[Int], cond:SetValue) = SumConstants(vars,cond)
}


object Prod{
  def apply(vars: Iterable[IntValue]) = new Prod(vars)
  def apply(vars: Array[IntValue], cond: SetValue) = ProdElements(vars, cond)
  def apply(vars: Array[Int], cond: SetValue) = ProdConstants(vars, cond)
}

/**
 * sum(vars)
 * @param vars is an iterable of IntVars
 * @author renaud.delandtsheer@cetic.be
 * */
class Sum(vars: Iterable[IntValue])
  extends IntInvariant(
    vars.foldLeft(0)((a:Int, b:IntValue) => a + b.value), vars.foldLeft(0)((acc, intvar) => DomainHelper.safeAddMin(acc,intvar.min)) to vars.foldLeft(0)((acc, intvar) => DomainHelper.safeAddMax(acc,intvar.max))){

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    this :+= NewVal - OldVal
  }

  override def checkInternals(c: Checker) {
    c.check(this.value == vars.foldLeft(0)((acc, intvar) => acc + intvar.value),
      Some("output.value == vars.foldLeft(0)((acc,intvar) => acc+intvar.value)"))
  }
}

/**
 * prod(vars)
 * @param vars is a set of IntVars
 * @author renaud.delandtsheer@cetic.be
 * */
class Prod(vars: Iterable[IntValue]) extends IntInvariant {
  assert(vars.size > 0, "Invariant prod declared with zero vars to multiply")

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  var NullVarCount: Int = vars.count(v => v.value == 0)
  var NonNullProd: Int = vars.foldLeft(1)((acc, intvar) => if (intvar.value == 0) { acc } else { acc * intvar.value })

  if (NullVarCount != 0) {
    this := 0
  } else {
    this := NonNullProd
  }

  //TODO: find better bound, this is far too much
  restrictDomain({
    val myMax = vars.foldLeft(1)((acc, intvar) => acc * (if (math.abs(intvar.max) > math.abs(intvar.min)) math.abs(intvar.max) else math.abs(intvar.min)))
    -myMax to myMax})

  @inline
  override def notifyIntChanged(v: ChangingIntValue, OldVal: Int, NewVal: Int) {
    assert(OldVal != NewVal)
    if (OldVal == 0 && NewVal != 0) {
      NullVarCount -= 1
      NonNullProd *= NewVal
    } else if (OldVal != 0 && NewVal == 0) {
      NullVarCount += 1
      NonNullProd = NonNullProd / OldVal
    } else {
      NonNullProd = NonNullProd / OldVal
      NonNullProd = NonNullProd * NewVal
    }
    if (NullVarCount == 0) {
      this := NonNullProd
    } else {
      this := 0
    }
  }

  override def checkInternals(c: Checker){
    var prod = 1
    for (v <- vars) prod *= v.value
    c.check(this.value == prod,
      Some("output.value (" + this.value + ") == prod (" + prod + ")"))
  }
}

/**
 * left - right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Minus(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, (l: Int, r: Int) => l - r, left.min - right.max to left.max - right.min) {
  assert(left != right)
}

/**
 * left + right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Sum2(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, ((l: Int, r: Int) => l + r), DomainHelper.safeAddMin(left.min,right.min) to DomainHelper.safeAddMax(left.max,right.max))

/**
 * left * right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Prod2(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, ((l: Int, r: Int) => l * r))

/**
 * Abs(Left - Right)
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Dist(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, ((l: Int, r: Int) => (l - r).abs), 0 to Int.MaxValue)

/**
 * left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual...
 * @author renaud.delandtsheer@cetic.be
 * */
case class Div(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, (l: Int, r: Int) => l / r)

/**
 * left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual...
 * @author renaud.delandtsheer@cetic.be
 * */
case class Mod(left: IntValue, right: IntValue)
  extends IntInt2Int(left, right, (l: Int, r: Int) => l - r * (l / r))

/**
 * abs(v) (absolute value)
 * where output and v are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Abs(v: IntValue)
  extends Int2Int(v, ((x: Int) => x.abs), (if (v.min <= 0) 0 else v.min) to v.max.max(-v.min))

/**
 * This invariant implements a step function. Values higher than pivot are mapped to ifval
 * values lower or equal to pivot are mapped to elseval
 * @author renaud.delandtsheer@cetic.be, suggested by Jean-Noël Monette
 *
 * @param x the IntVar parameter of the invariant
 * @param pivot the pivot value
 * @param thenval the value returned when x > pivot
 * @param elseval the value returned when x <= pivot
 */
case class Step(x: IntValue, pivot: Int = 0, thenval: Int = 1, elseval: Int = 0)
  extends Int2Int(x, (a: Int) => if (a > pivot) thenval else elseval, 0 to 1)

/**
 * This invariant implements the identity function within the min-max range.
 * values lower tham min result to min
 * values higher tham max result to max
 * @author renaud.delandtsheer@cetic.be
 * @param x
 * @param minBound
 * @param maxBound
 */
case class Bound(x: IntValue, minBound:Int, maxBound:Int)
  extends Int2Int(x, (a: Int) => if (a < minBound) minBound else if (a > maxBound) maxBound else a, math.max(minBound,x.min) to math.min(maxBound,x.max))
