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
/******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.constraints.lib.global

import collection.immutable.SortedMap
import oscar.cbls.constraints.core.Constraint
import oscar.cbls.invariants.lib.logic.{Cluster, IntElement}
import oscar.cbls.invariants.lib.numeric.{Sum, SumElements}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
;

/**This is the standard bin packing constraint
 *
 * @param items the items, designing the bins they are placed into
 * @param itemsizes the size of the items
 * @param binsizes the max size of the available bins
  * @author renaud.delandtsheer@cetic.be
 */
case class MultiKnapsack(items: Array[CBLSIntVar], itemsizes: Array[CBLSIntVar], binsizes:Array[CBLSIntVar])
  extends Constraint {

  model = InvariantHelper.findModel(items)

  assert(items.map(_.minVal).min == 0, "bin 0 must be included in possible bins of items")
  assert(items.map(_.minVal).max <= binsizes.length-1, "the range of item bins should be not bigger than the available bins")
  assert(items.length == itemsizes.length)

  registerConstrainedVariables(items)
  registerConstrainedVariables(itemsizes)
  registerConstrainedVariables(binsizes)

  finishInitialization()

  val bincontents:Array[CBLSSetVar] = Cluster.MakeDense(items).clusters
  val binfilling:Array[CBLSIntVar] = bincontents.map(bincontent => SumElements(itemsizes,bincontent).toIntVar)

  val binviolations:Array[CBLSIntVar] = (
    for (binid <- binsizes.indices)
    yield (binfilling(binid) le binsizes(binid)).violation).toArray

  val itemviolations:Array[CBLSIntVar] = items.map(itemval =>  binviolations.element(itemval))

  val Violation:CBLSIntVar = Sum(binviolations).toIntVar

  val Violations:SortedMap[CBLSIntVar,CBLSIntVar] = {
    var acc = SortedMap.empty[CBLSIntVar,CBLSIntVar]
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
  override def violation = Violation

  /**The violation of an item is the excess of the bin it is located into,
   * The violation of a bin is the excess in the bin
   */
  override def violation(v: Variable): CBLSIntVar = {
    val tmp:CBLSIntVar = Violations.getOrElse(v.asInstanceOf[CBLSIntVar],null)
    assert(tmp != null)
    tmp
  }

  /** To override whenever possible to spot errors in invariants.
    * this will be called for each invariant after propagation is performed.
    * It requires that the Model is instantiated with the variable debug set to true.
    */
  override def checkInternals(c: Checker) {
    c.check(true,Some("nothing to check, invariant is discharged"))
  }
}
