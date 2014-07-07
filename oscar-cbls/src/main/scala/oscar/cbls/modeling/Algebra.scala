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
package oscar.cbls.modeling

/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 *     Contributed to by Florent Ghilain
 ******************************************************************************/

import oscar.cbls.constraints.lib.basic._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.set._
import collection.immutable.SortedSet
import oscar.cbls.invariants.lib.logic.{SetElement, Elements, IntElement}
import oscar.cbls.invariants.lib.numeric._
import collection.Iterator
import language.implicitConversions
import oscar.cbls.invariants.lib.numeric.Mod
import oscar.cbls.constraints.lib.basic.G
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.cbls.invariants.lib.logic.Elements
import oscar.cbls.invariants.lib.numeric.Div
import oscar.cbls.constraints.lib.basic.GE
import oscar.cbls.invariants.lib.numeric.Prod
import oscar.cbls.constraints.lib.basic.EQ
import oscar.cbls.constraints.lib.basic.L
import oscar.cbls.invariants.lib.logic.SetElement
import oscar.cbls.constraints.lib.basic.NE
import oscar.cbls.invariants.core.computation.CBLSIntConst
import oscar.cbls.constraints.lib.basic.LE
import oscar.cbls.invariants.lib.numeric.Minus
import oscar.cbls.invariants.core.computation.CBLSSetConst
import oscar.cbls.invariants.lib.numeric.Mod
import oscar.cbls.constraints.lib.basic.G
import oscar.cbls.invariants.lib.logic.IntElement
import oscar.cbls.invariants.lib.numeric.Sum2
import oscar.cbls.invariants.lib.set.Inter
import oscar.cbls.invariants.lib.logic.Elements
import oscar.cbls.invariants.lib.numeric.Div
import oscar.cbls.constraints.lib.basic.GE
import oscar.cbls.invariants.lib.set.Union
import oscar.cbls.invariants.lib.numeric.Prod
import oscar.cbls.constraints.lib.basic.EQ
import oscar.cbls.invariants.lib.set.Interval
import oscar.cbls.constraints.lib.basic.L
import oscar.cbls.invariants.lib.logic.SetElement
import oscar.cbls.constraints.lib.basic.NE
import oscar.cbls.invariants.lib.set.Diff
import oscar.cbls.invariants.core.computation.CBLSIntConst
import oscar.cbls.constraints.lib.basic.LE
import oscar.cbls.invariants.lib.numeric.Minus
import oscar.cbls.invariants.core.computation.CBLSSetConst

/**Include this object whenever you want to use concise notation
 * It provides the following infix operators for IntVars: plus minus times, div, ==: !=: <<: >>: >=: <=:
  * @author renaud.delandtsheer@cetic.be
  * */
object Algebra extends AlgebraTrait{
}

trait AlgebraTrait{
  class ShiftedRange(val start:Int, val end:Int, val startBy:Int, val step:Int = 1) extends Iterable[Int]{
    if(!(Range.inclusive(start,end,step).contains(startBy))) throw new Exception("ShiftedRange must contain startBy value " + this)
    if(step != 1) throw new Exception("only step of 1 is supported in ShirtedRange")

    //include the at Value
    private def unfold(at:Int):List[Int] = {
      if(at == end){
        unfold (start)
      }else if(getNextValue(at) == startBy){
        List(at)
      }else{
        at :: unfold(at+1)
      }
    }
    
    def getNextValue(a:Int) = {
      if(a == end) start
      else a+1
    }

    override def iterator: Iterator[Int] = new ShiftedRangeIterator(this)

    override def toList: List[Int] = unfold(startBy)

    override def toArray[B >: Int](implicit evidence$1: scala.reflect.ClassTag[B]): Array[B] = toList.toArray

    override def toString(): String = "ShiftedRange(" + toList + ")"
  }

  class ShiftedRangeIterator(val s:ShiftedRange) extends Iterator[Int]{
    var currentValue = s.startBy

    def hasNext: Boolean = (s.getNextValue(currentValue) != s.startBy)

    def next(): Int = {
      val tmp = currentValue
      currentValue = s.getNextValue(currentValue)
      tmp
    }
  }

  // implicit conversion of Range towards a RangeHotRestart
  implicit def instrumentRange(r:Range):InstrumentedRange = new InstrumentedRange(r)

  class InstrumentedRange(r:Range){
    def startBy (start:Int)  =  new ShiftedRange(r.head, r.last,start:Int, r.step)
  }

  implicit def InstrumentIntVar(v: CBLSIntVar): InstrumentedIntVar = new InstrumentedIntVar(v)

  implicit def InstrumentIntInvariant(i: IntInvariant): InstrumentedIntVar = InstrumentIntVar(i.toIntVar)

  implicit def InstrumentInt(a: Int): InstrumentedIntVar = InstrumentIntVar(CBLSIntConst(a))

  class InstrumentedIntVar(x: CBLSIntVar) {
    def +(v: CBLSIntVar): IntInvariant = Sum2(x, v)

    def -(v: CBLSIntVar): IntInvariant = Minus(x, v)

    def *(v: CBLSIntVar): IntInvariant = Prod(List(x, v))

    def /(v: CBLSIntVar): IntInvariant = Div(x, v)

    def %(v: CBLSIntVar): IntInvariant = Mod(x, v)

    def ===(v: CBLSIntVar) = new EQ(x, v)

    def !==(v: CBLSIntVar) = new NE(x, v)

    def >>=(v: CBLSIntVar) = new G(x, v)

    def <<=(v: CBLSIntVar) = new L(x, v)

    def >==(v: CBLSIntVar) = new GE(x, v)

    def le(v: CBLSIntVar) = new LE(x, v)

    /** creates a IntSEt maintained as the inclusive interval between te two variable
      * see [[oscar.cbls.invariants.lib.set.Interval]]
      * @param v
      * @return
      */
    def TO (v:CBLSIntVar) = new Interval(x,v)
  }

  implicit def InstrumentIntSetVar(v: CBLSSetVar): InstrumentedIntSetVar = new InstrumentedIntSetVar(v)

  implicit def InstrumentIntSetInvariant(i: SetInvariant): InstrumentedIntSetVar = InstrumentIntSetVar(i.toSetVar)

  implicit def InstrumentIntSet(a: SortedSet[Int]): InstrumentedIntSetVar = InstrumentIntSetVar(CBLSSetConst(a))

  class InstrumentedIntSetVar(x: CBLSSetVar) {
    def union(v: CBLSSetVar): SetInvariant = Union(x, v)

    def inter(v: CBLSSetVar): SetInvariant = Inter(x, v)

    def minus(v: CBLSSetVar): SetInvariant = Diff(x, v)

    def map(fun:Int=>Int, myMin:Int = Int.MinValue, myMax:Int = Int.MaxValue) = SetMap(x,fun,myMin,myMax)

    }

  implicit def InstrumentArrayOfIntVar(inputarray: Array[CBLSIntVar]): InstrumentedArrayOfIntVar
  = new InstrumentedArrayOfIntVar(inputarray)

  class InstrumentedArrayOfIntVar(inputarray: Array[CBLSIntVar]) {
    def element(index: CBLSIntVar): CBLSIntVar = IntElement(index, inputarray)

    def elements(index: CBLSSetVar): CBLSSetVar = Elements(index, inputarray)
  }

  implicit def InstrumentArrayOfIntSetVar(inputarray: Array[CBLSSetVar]): InstrumentedArrayOfIntSetVar
  = new InstrumentedArrayOfIntSetVar(inputarray)

  class InstrumentedArrayOfIntSetVar(inputarray: Array[CBLSSetVar]) {
    def apply(index: CBLSIntVar): CBLSSetVar = SetElement(index, inputarray)
  }

  implicit def arrayOfIntTOArrayOfIntConst(a:Array[Int]):Array[CBLSIntVar] = a.map(CBLSIntConst(_))
}

