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

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.invariants.lib.logic

import oscar.cbls.invariants.core.computation.{Invariant, IntVar}

/**Maintains a count of the indexes of array: count(j) = #{i in index of values | values[i] == j}
 * This is considered as a dense count because counts is an array and must cover all the possibles values of the values in the array ''values''
 * */
case class DenseCount(values:Array[IntVar], counts:Array[IntVar]) extends Invariant {

  for (v <- values.indices) registerStaticAndDynamicDependency(values(v),v)

  for (count <- counts){count := 0}

  for(v <- values.indices){
    counts(values(v).value) :+= 1
  }

  finishInitialization()

  for(c <- counts){c.setDefiningInvariant(this)}

  @inline
  override def notifyIntChanged(v:IntVar,index:Int,OldVal:Int,NewVal:Int){
    assert(values(index) == v)
    counts(OldVal) :-= 1
    counts(NewVal) :+= 1
  }
}

