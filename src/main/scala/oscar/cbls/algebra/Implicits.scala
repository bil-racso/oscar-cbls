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

/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 */

package oscar.cbls.algebra

import oscar.cbls.invariants.lib.numeric.{Div, Prod, Minus, Sum2}
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.set.{Inter, Diff, Union}
import collection.immutable.SortedSet

/**Include this object whenever you want to use concise notation
 * It provides the following ginfix operators for IntVars: plus minus times, div, ==: !=: <<: >>: >=: <=:
 */
object Implicits{

  implicit def InstrumentIntVar(v:IntVar):InstrumentedIntVar = new InstrumentedIntVar(v)
  implicit def InstrumentIntInvariant(i:IntInvariant):InstrumentedIntVar = InstrumentIntVar(i.toIntVar)
  implicit def InstrumentInt(a:Int):InstrumentedIntVar = InstrumentIntVar(IntConst(a))

  class InstrumentedIntVar(x:IntVar){
    def plus (v:IntVar):IntInvariant = Sum2(x,v)

    def minus (v:IntVar):IntInvariant = Minus(x,v)
    def times (v:IntVar):IntInvariant = Prod(List(x,v))

    def div (v:IntVar):IntInvariant = Div(x,v)
    
    def ==:(v:IntVar):Constraint = EQ(x,v)
    def !=:(v:IntVar):Constraint = NE(x,v)
    def >>:(v:IntVar):Constraint = G(x,v)
    def <<:(v:IntVar):Constraint = L(x,v)
    def >=:(v:IntVar):Constraint = GE(x,v)
    def <=:(v:IntVar):Constraint = LE(x,v)
  }

  implicit def InstrumentIntSetVar(v:IntSetVar):InstrumentedIntSetVar = new InstrumentedIntSetVar(v)
  implicit def InstrumentIntSetInvariant(i:IntSetInvariant):InstrumentedIntSetVar = InstrumentIntSetVar(i.toIntSetVar)
  implicit def InstrumentIntSet(a:SortedSet[Int]):InstrumentedIntSetVar = InstrumentIntSetVar(IntSetConst(a))

  class InstrumentedIntSetVar(x:IntSetVar){
    def union (v:IntSetVar):IntSetInvariant = Union(x,v)
    def inter (v:IntSetVar):IntSetInvariant = Inter(x,v)
    def minus (v:IntSetVar):IntSetInvariant = Diff(x,v)
  }
}
