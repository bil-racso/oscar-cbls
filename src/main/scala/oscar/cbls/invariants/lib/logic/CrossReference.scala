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

package oscar.cbls.invariants.lib.logic
/**This package proposes a set of logic invariants, which are used to define the structure of the problem*/


import collection.immutable.SortedSet
import oscar.cbls.invariants.core.computation.Invariant._
import oscar.cbls.invariants.core.computation.{Invariant, IntSetVar, IntVar}

/**maintains the reverse references. Referencing(i) = {j | Reference(j) includes i}
 * */
case class DenseRef(references:Array[IntSetVar], referencing:Array[IntSetVar]) extends Invariant {

  for (v <- references.indices) registerStaticAndDynamicDependency(references(v),v)

  finishInitialization()

  for(c <- referencing){c.setDefiningInvariant(this); c.setValue(SortedSet.empty)}

  for(v <- references.indices){
    for (r <- references(v).getValue()){
      referencing(r).insertValue(v)
    }
  }

  @inline
  override def notifyInsertOn(v: IntSetVar, i: Int, value: Int){
    referencing(value).insertValue(i)
  }

  @inline
  override def notifyDeleteOn(v: IntSetVar, i: Int, value: Int){
    referencing(value).deleteValue(i)
  }
}
