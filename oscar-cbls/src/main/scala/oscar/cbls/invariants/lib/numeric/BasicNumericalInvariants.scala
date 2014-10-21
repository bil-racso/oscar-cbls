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

import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.core.propagation.Checker;

object Sum{
  def apply(vars: Iterable[CBLSIntVar]):Sum = new Sum(vars)
  def apply(vars: Array[CBLSIntVar], cond: CBLSSetVar):SumElements = SumElements(vars, cond)
}


object Prod{
  def apply(vars: Iterable[CBLSIntVar]):Prod = new Prod(vars)
  def apply(vars: Array[CBLSIntVar], cond: CBLSSetVar):ProdElements = ProdElements(vars, cond)
}

/**
 * sum(vars)
 * @param vars is an iterable of IntVars
 * @author renaud.delandtsheer@cetic.be
 * */
class Sum(vars: Iterable[CBLSIntVar]) extends IntInvariant {
//actually, it works fine with zero vars.
//  assert(vars.size > 0, "Invariant + declared with zero vars to sum up")

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  def myMin = vars.foldLeft(0)((acc, intvar) => acc + intvar.minVal)
  def myMax = vars.foldLeft(0)((acc, intvar) => acc + intvar.maxVal)

  var output: CBLSIntVar = null

  override def setOutputVar(v: CBLSIntVar) {
    output = v
    output.setDefiningInvariant(this)
    output := vars.foldLeft(0)((a, b) => a + b.value)
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, OldVal: Int, NewVal: Int) {
    output :+= NewVal - OldVal
  }

  override def checkInternals(c: Checker) {
    c.check(output.value == vars.foldLeft(0)((acc, intvar) => acc + intvar.value),
      Some("output.value == vars.foldLeft(0)((acc,intvar) => acc+intvar.value)"))
  }
}

/**
 * prod(vars)
 * @param vars is a set of IntVars
 * @author renaud.delandtsheer@cetic.be
 * */
class Prod(vars: Iterable[CBLSIntVar]) extends IntInvariant {
  assert(vars.size > 0, "Invariant prod declared with zero vars to multiply")

  for (v <- vars) registerStaticAndDynamicDependency(v)
  finishInitialization()

  var NullVarCount: Int = vars.count(v => v.value == 0)
  var NonNullProd: Int = vars.foldLeft(1)((acc, intvar) => if (intvar.value == 0) { acc } else { acc * intvar.value })

  var output: CBLSIntVar = null

  //TODO: find better bound, this is far too much
  def myMax = vars.foldLeft(1)((acc, intvar) => acc * (if (math.abs(intvar.maxVal) > math.abs(intvar.minVal)) math.abs(intvar.maxVal) else math.abs(intvar.minVal)))
  def myMin = -myMax

  override def setOutputVar(v: CBLSIntVar) {
    output = v
    output.setDefiningInvariant(this)
    if (NullVarCount != 0) {
      output := 0
    } else {
      output := NonNullProd
    }
  }

  @inline
  override def notifyIntChanged(v: CBLSIntVar, OldVal: Int, NewVal: Int) {
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
      output := NonNullProd
    } else {
      output := 0
    }
  }

  override def checkInternals(c: Checker){
    var prod = 1
    for (v <- vars) prod *= v.value
    c.check(output.value == prod,
      Some("output.value (" + output.value + ") == prod (" + prod + ")"))
  }
}

/**
 * left - right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Minus(left: CBLSIntVar, right: CBLSIntVar)
  extends IntInt2Int(left, right, ((l: Int, r: Int) => l - r), left.minVal - right.maxVal, left.maxVal - right.minVal) {
  assert(left != right)
}

/**
 * left + right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Sum2(left: CBLSIntVar, right: CBLSIntVar)
  extends IntInt2Int(left, right, ((l: Int, r: Int) => l + r), left.minVal + right.minVal, left.maxVal + right.maxVal)

/**
 * left * right
 * where left, right, and output are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Prod2(left: CBLSIntVar, right: CBLSIntVar)
  extends IntInt2Int(left, right, ((l: Int, r: Int) => l * r), Int.MinValue, Int.MaxValue)

/**
 * left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual...
 * @author renaud.delandtsheer@cetic.be
 * */
case class Div(left: CBLSIntVar, right: CBLSIntVar)
  extends IntInt2Int(left, right, (l: Int, r: Int) => l / r)

/**
 * left / right
 * where left, right, and output are IntVar
 * do not set right to zero, as usual...
 * @author renaud.delandtsheer@cetic.be
 * */
case class Mod(left: CBLSIntVar, right: CBLSIntVar)
  extends IntInt2Int(left, right, (l: Int, r: Int) => l - r * (l / r))

/**
 * abs(v) (absolute value)
 * where output and v are IntVar
 * @author renaud.delandtsheer@cetic.be
 * */
case class Abs(v: CBLSIntVar)
  extends Int2Int(v, ((x: Int) => x.abs), (if (v.minVal <= 0) 0 else v.minVal), v.maxVal.max(-v.minVal))

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
case class Step(x: CBLSIntVar, pivot: Int = 0, thenval: Int = 1, elseval: Int = 0)
  extends Int2Int(x, (a: Int) => if (a > pivot) thenval else elseval, 0, 1)

/**
 * This invariant implements the identity function within the min-max range.
 * values lower tham min result to min
 * values higher tham max result to max
 * @author renaud.delandtsheer@cetic.be
 * @param x
 * @param min
 * @param max
 */
case class Bound(x: CBLSIntVar, min:Int, max:Int)
  extends Int2Int(x, (a: Int) => if (a < min) min else if (a > max) max else a,min, max)

