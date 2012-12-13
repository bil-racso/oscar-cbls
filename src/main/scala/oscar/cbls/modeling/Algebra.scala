package oscar.cbls.modeling

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.constraints.core.Constraint
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.set.{Inter, Diff, Union}
import collection.immutable.SortedSet
import oscar.cbls.invariants.lib.logic.{IntSetElement, IntElements, IntElement}
import oscar.cbls.invariants.lib.numeric._

/**Include this object whenever you want to use concise notation
 * It provides the following ginfix operators for IntVars: plus minus times, div, ==: !=: <<: >>: >=: <=:
 */
object Algebra {

  class RangeHotRestart(start:Int,end:Int,step:Int) extends IndexedSeq[Int]{
    def this(start:Int,end:Int) = this(start,end,1)
    val length = end - start
    def apply(n:Int) = (start + step*n)%length
  }
  object RangeHotRestart{
    def apply(start:Int,end:Int,step:Int) = new RangeHotRestart(start,end,step)
    def apply(start:Int,end:Int) = new RangeHotRestart(start,end)
  }
  // implicit conversion of Range towards a RangeHotRestart
  implicit def InstrumentedRange(r:Range):InstrumentedRange = new InstrumentedRange(r)

  class InstrumentedRange(r:Range){
    def startBy (start:Int)  = {
      if(r.contains(start)) RangeHotRestart(start,start+r.length)
      else throw new IllegalArgumentException("Only values between "+r.start+" and "+(r.end-1))
    }
  }

  implicit def InstrumentIntVar(v: IntVar): InstrumentedIntVar = new InstrumentedIntVar(v)

  implicit def InstrumentIntInvariant(i: IntInvariant): InstrumentedIntVar = InstrumentIntVar(i.toIntVar)

  implicit def InstrumentInt(a: Int): InstrumentedIntVar = InstrumentIntVar(IntConst(a))

  class InstrumentedIntVar(x: IntVar) {
    def +(v: IntVar): IntInvariant = Sum2(x, v)

    def -(v: IntVar): IntInvariant = Minus(x, v)

    def *(v: IntVar): IntInvariant = Prod(List(x, v))

    def /(v: IntVar): IntInvariant = Div(x, v)

    def %(v: IntVar): IntInvariant = Mod(x, v)

    def ===(v: IntVar) = new EQ(x, v)

    def !==(v: IntVar) = new NE(x, v)

    def >>=(v: IntVar) = new G(x, v)

    def <<=(v: IntVar) = new L(x, v)

    def >==(v: IntVar) = new GE(x, v)

    def le(v: IntVar) = new LE(x, v)
  }

  implicit def InstrumentIntSetVar(v: IntSetVar): InstrumentedIntSetVar = new InstrumentedIntSetVar(v)

  implicit def InstrumentIntSetInvariant(i: IntSetInvariant): InstrumentedIntSetVar = InstrumentIntSetVar(i.toIntSetVar)

  implicit def InstrumentIntSet(a: SortedSet[Int]): InstrumentedIntSetVar = InstrumentIntSetVar(IntSetConst(a))

  class InstrumentedIntSetVar(x: IntSetVar) {
    def union(v: IntSetVar): IntSetInvariant = Union(x, v)

    def inter(v: IntSetVar): IntSetInvariant = Inter(x, v)

    def minus(v: IntSetVar): IntSetInvariant = Diff(x, v)
  }

  implicit def InstrumentArrayOfIntVar(inputarray: Array[IntVar]): InstrumentedArrayOfIntVar
  = new InstrumentedArrayOfIntVar(inputarray)

  class InstrumentedArrayOfIntVar(inputarray: Array[IntVar]) {
    def element(index: IntVar): IntVar = IntElement(index, inputarray)

    def elements(index: IntSetVar): IntSetVar = IntElements(index, inputarray)
  }

  implicit def InstrumentArrayOfIntSetVar(inputarray: Array[IntSetVar]): InstrumentedArrayOfIntSetVar
  = new InstrumentedArrayOfIntSetVar(inputarray)

  class InstrumentedArrayOfIntSetVar(inputarray: Array[IntSetVar]) {
    def apply(index: IntVar): IntSetVar = IntSetElement(index, inputarray)
  }

}

