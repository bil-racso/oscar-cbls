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

package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.lib.logic.{Cluster, IntElement}
import oscar.cbls.invariants.lib.numeric.{Sum, SumElements}
import oscar.cbls.algebra.Implicits._
import oscar.cbls.invariants.core.computation._
;

//TODO: test this
/**This is the standard bin packing constraint
 * @param items the items, designing the bins they are placed into
 * @param itemsizes the size of the items
 * @param binsizes the max size of the available bins
 */
case class MultiKnapsack(items: Array[IntVar], itemsizes: Array[IntVar], binsizes:Array[IntVar])
  extends Constraint {

  registerConstrainedVariablesAll(items)
  registerConstrainedVariablesAll(itemsizes)
  registerConstrainedVariablesAll(binsizes)

  finishInitialization()

  val bincontents:Array[IntSetVar] = Cluster.MakeDense(items).clusters
  val binfilling:Array[IntVar] = {
    val tmp = bincontents.map(bincontent => SumElements(null,bincontent))
    BulkLoad(tmp,itemsizes)
    tmp.map(sumelement => sumelement.toIntVar)
  }
  val binviolations:Array[IntVar] = (
    for (binid <- binsizes.indices)
    yield (binfilling(binid) <=: binsizes(binid)).getViolation).toArray

  val itemviolations:Array[IntVar] = {
    val tmp = items.map(itemval => IntElement(itemval,null))
    BulkLoad(tmp,binviolations)
    tmp.map(intelement => intelement.toIntVar)
  }

  val Violation:IntVar = Sum(binviolations).toIntVar

  val Violations:SortedMap[IntVar,IntVar] = {
    var acc = SortedMap.empty[IntVar,IntVar]
    for(itemid <- items.indices){
      acc += ((items(itemid),itemviolations(itemid)))
      acc += ((itemsizes(itemid),itemviolations(itemid)))
    }
    for(binid <- binsizes.indices){
      acc += ((binsizes(binid),binviolations(binid)))
    }
    acc
  }

  /**The violation of the constraint is the sum on all excess in all bins.
   */
  override def getViolation = Violation

  /**The violation of an item is the excess of the bin it is located into,
   * The violation of a bin is the excess in the bin
   */
  override def getViolation(v: Variable): IntVar = {
    val tmp:IntVar = Violations.getOrElse(v.asInstanceOf[IntVar],null)
    assert(tmp != null)
    tmp
  }
}
