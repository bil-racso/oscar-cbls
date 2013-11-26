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
 ******************************************************************************/


package oscar.cbls.invariants.lib.minmax
/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/

import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.algo.heap._
import collection.immutable.SortedSet

import oscar.cbls.invariants.lib.logic._

abstract class MiaxLin(vars: SortedSet[IntVar]) extends IntInvariant {
  require(vars.size > 0, "Invariant " + name + " declared with zero vars to max")

  def name: String
  var output: IntVar = null

  for(v <- vars)registerStaticAndDynamicDependency(v)
  finishInitialization()

  override def myMax = vars.foldLeft(vars.head.maxVal)((acc, intvar) => if (Better(intvar.maxVal,acc)) intvar.maxVal else acc)
  override def myMin = vars.foldLeft(vars.head.minVal)((acc, intvar) => if (Better(intvar.minVal,acc)) intvar.minVal else acc)

  var MiaxCount: Int = 0

  def Better(a: Int, b: Int): Boolean //true if a is strictly more in the direction of the invariant that b

  private def LoadNewMiax() {
    var CurrentMiax: Int = vars.head.value
    MiaxCount = 1
    vars.foreach(v => {
      if (v.value == CurrentMiax) {
        MiaxCount += 1
      } else if (Better(v.value, CurrentMiax)) {
        MiaxCount = 1
        CurrentMiax = v.value
      }
    })
    output := CurrentMiax
  }

  override def setOutputVar(v: IntVar) {
      output = v
      output.setDefiningInvariant(this)
      LoadNewMiax()
  }

  override def notifyIntChanged(v: IntVar, OldVal: Int, NewVal: Int) {
    assert(vars.contains(v), name + " notified for not interesting var")
    val MiaxVal = output.getValue(true)
    if (OldVal == MiaxVal && Better(MiaxVal, NewVal)) {
      MiaxCount -= 1
      if (MiaxCount == 0) LoadNewMiax() //this is where we pay the price.
    } else if (Better(NewVal, MiaxVal)) {
      MiaxCount = 1
      output := NewVal
    } else if (Better(MiaxVal, OldVal) && NewVal == MiaxVal) {
      MiaxCount += 1
    }
  }
}

/** maintains output = Max(vars)
 * where
 * * output is an IntVar
 * * on is a set of IntVar
 * update is O(n)
 * */
case class MaxLin(vars: SortedSet[IntVar]) extends MiaxLin(vars) {
  override def name = "MaxLin"

  override def Better(a: Int, b: Int): Boolean = (a > b)
}

/** maintains output = Min(vars)
 * where
 * * output is an IntVar
 * * on is a set of IntVar
 * update is O(n)
 * */
case class MinLin(vars: SortedSet[IntVar]) extends MiaxLin(vars) {
  override def name = "MinLin"

  override def Better(a: Int, b: Int): Boolean = (a < b)

}

abstract class Miax(vars: SortedSet[IntVar]) extends IntInvariant{
  def name: String

  override def myMax = vars.foldLeft(vars.head.maxVal)((acc, intvar) => if (Better(intvar.maxVal,acc)) intvar.maxVal else acc)
  override def myMin = vars.foldLeft(vars.head.minVal)((acc, intvar) => if (Better(intvar.minVal,acc)) intvar.minVal else acc)

  for(v <- vars)registerStaticAndDynamicDependency(v)
  finishInitialization()

  def Ord(v: IntVar): Int
  def Better(a:Int,b:Int):Boolean

  //TODO: this is awfully slow, but what can you do with a SortedSet?
  val h: BinomialHeapWithMove[IntVar] = new BinomialHeapWithMove[IntVar](Ord, vars.size)

  for (v <- vars) {h.insert(v)}

  var output: IntVar = null

  override def setOutputVar(v: IntVar) {
      output = v
    output.setDefiningInvariant(this)
      output := h.getFirst.value
  }

  override def notifyIntChanged(v: IntVar, OldVal: Int, NewVal: Int) {
    assert(vars.contains(v), name + " notified for not interesting var")
    h.notifyChange(v)
    output := h.getFirst.value
  }
}

/** maintains output = Min(vars)
 * deprecated: use MinArray
 * where
 * * output is an IntVar
 * update is O(n*n)
 * */
@deprecated("use the MinArray instead","always")
case class Min(vars: SortedSet[IntVar]) extends Miax(vars) {
  assert(vars.size > 0, "Invariant Min declared with zero vars to min")

  override def name = "Min"

  override def Ord(v: IntVar): Int = v.value

  override def Better(a:Int,b:Int):Boolean = a < b
}

/** maintains output = Max(vars)
 * deprecated use MaxArray
 * where
 * * output is an IntVar
 * update is O(n*n)
 * */
@deprecated("use the MaxArray instead","always")
case class Max(vars: SortedSet[IntVar]) extends Miax(vars) {
  assert(vars.size > 0, "Invariant Max declared with zero vars to max")
  override def name = "Max"

  override def Ord(v: IntVar): Int = -v.value

  override def Better(a:Int,b:Int):Boolean = a > b
}



/** maintains output = Max(a,b)
 * where output, a, and b are an IntVar
 * use this if you only have two variables to max, otherwise, refer to log implementations
 * */
case class Max2(a: IntVar, b: IntVar)
  extends IntVarIntVar2IntVarFun(a, b, ((x: Int, y: Int) => x.max(y)), a.minVal.max(b.minVal), a.maxVal.max(b.maxVal))

/** maintains output = Min(a,b)
 * where output, a, and b are an IntVar
 * use this if you only have two variables to max, otherwise, refer to log implementations
 * */
case class Min2(a: IntVar, b: IntVar)
  extends IntVarIntVar2IntVarFun(a, b, ((x: Int, y: Int) => x.min(y)), a.minVal.min(b.minVal), a.maxVal.min(b.maxVal))
