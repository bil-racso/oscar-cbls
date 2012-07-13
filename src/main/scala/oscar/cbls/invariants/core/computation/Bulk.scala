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

package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.propagation.{BulkPropagator, PropagationElement}


/**Invariants over arrays can implement this trait to make it possible to bulk load their dependencies*/
trait Bulked[VarType,BulkedComputationResult] extends Invariant{
  /**through this method, a bulked invariant receives the bulked array of variables
   * there is no need to register static dependencies here, this is handled by the BulkLoad
   * Instead, one should implement the business specific initialization, and the registration to the dynamic graph here.
   * Notice that the finishInitialization method should already be called during the constructor,
   * so do not call it from this method.
   * @param bulkedVar the bulked array of variables
   */
  def BulkLoad(bulkedVar: Array[VarType], bcr: BulkedComputationResult)

  def performBulkComputation(bulkedVar: Array[VarType]): BulkedComputationResult = null.asInstanceOf[BulkedComputationResult]

  final def registerBulkDependency(bulk: Bulk){
    this.registerStaticallyListenedElement(bulk)
  }
}

/**A bulk load is to use when there are a set of invariants that all listen statically to the same array of variables
 * but whose dynamic dependencies are really sparse
 * in case such dependencies are bulk loaded,
 * we create an instance of Bulk between the variables and invariants such that
 * the bulk statically listens to all variables and the invariants statically listen to the bulk.
 * In this setting, the number of dependencies is O(N) instead of O(N*N)
 * Also, if you use this, please ensure that the bulk loaded invariants will not waste their time iterating
 * on the bulked variables eg for finding the min and max value of these variables. If you need to compute some value over the bulked variables,
 * you can use the method performBulkComputation in [[oscar.cbls.invariants.core.computation.Bulk]] for good efficiency.
 *
 */
object BulkLoad{
  def apply[VarType <: Variable,BulkedComputationResult](invariants: Iterable[Bulked[VarType,BulkedComputationResult]], Variables: Array[VarType]){
    val IncredibleBulk = new Bulk(InvariantHelper.FindModel(Variables),Variables)
    val bcr = invariants.head.performBulkComputation(Variables)
    for (i <- invariants) {
      i.registerBulkDependency(IncredibleBulk)
      i.BulkLoad(Variables,bcr)
    }
  }
}

/**This is the node that is put in the propagation graph
 * used by BulkLoad only*/
class Bulk(m:Model, d: Iterable[PropagationElement]) extends Invariant with BulkPropagator{
  for (dd <- d) registerStaticallyListenedElement(dd)
  finishInitialization(m)
  override def getDotNode:String = "[label=\"Bulk\" shape=diamond]"
}
