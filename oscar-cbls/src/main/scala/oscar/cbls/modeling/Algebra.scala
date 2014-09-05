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

import oscar.cbls.constraints.lib.basic.{EQ, G, GE, L, LE, NE}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.logic.{Elements, IntElement, SetElement}
import oscar.cbls.invariants.lib.numeric.{Div, Minus, Mod, Prod, Sum2}
import oscar.cbls.invariants.lib.set._
import oscar.cbls.search.algo.InstrumentedRange

import scala.collection.immutable.SortedSet
import scala.language.implicitConversions

/**Include this object whenever you want to use concise notation
 * It provides the following infix operators for IntVars: plus minus times, div, ==: !=: <<: >>: >=: <=:
  * @author renaud.delandtsheer@cetic.be
  * */
object Algebra extends AlgebraTrait{
}

trait AlgebraTrait{

  // implicit conversion of Range towards a RangeHotRestart
  implicit def instrumentRange(r:Range):InstrumentedRange = new InstrumentedRange(r)

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
  implicit def arrayOfIntInvariantArrayOfIntVar(a:Array[IntInvariant]):Array[CBLSIntVar] = a.map(_.toIntVar)
}

