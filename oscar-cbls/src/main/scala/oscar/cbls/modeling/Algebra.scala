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
import oscar.cbls.invariants.lib.set.{Interval, Inter, Diff, Union}
import collection.immutable.SortedSet
import oscar.cbls.invariants.lib.logic.{SetElement, Elements, IntElement}
import oscar.cbls.invariants.lib.numeric._
import collection.Iterator
import language.implicitConversions

/**Include this object whenever you want to use concise notation
 * It provides the following infix operators for IntVars: plus minus times, div, ==: !=: <<: >>: >=: <=:
 */
object Algebra extends AlgebraTrait{
}

trait AlgebraTrait{
  class ShiftedRange(override val start:Int, override val end:Int, val startBy:Int, override val step:Int = 1)
    extends Range(start,end,step) {
    if(!(this.contains(startBy))) throw new Exception("ShiftedRange must contain startBy value " + this)
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

    override def toIterator: Iterator[Int] = new ShiftedRangeIterator(this)

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
    def startBy (start:Int)  =  new ShiftedRange(r.start, r.end,start:Int, r.step)
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

  implicit def InstrumentIntSetInvariant(i: SetInvariant): InstrumentedIntSetVar = InstrumentIntSetVar(i.toIntSetVar)

  implicit def InstrumentIntSet(a: SortedSet[Int]): InstrumentedIntSetVar = InstrumentIntSetVar(CBLSSetConst(a))

  class InstrumentedIntSetVar(x: CBLSSetVar) {
    def union(v: CBLSSetVar): SetInvariant = Union(x, v)

    def inter(v: CBLSSetVar): SetInvariant = Inter(x, v)

    def minus(v: CBLSSetVar): SetInvariant = Diff(x, v)
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


}

