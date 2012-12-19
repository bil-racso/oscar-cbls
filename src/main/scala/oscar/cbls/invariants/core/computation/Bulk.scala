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

package oscar.cbls.invariants.core.computation

import oscar.cbls.invariants.core.propagation.{BulkPropagator, PropagationElement}
import collection.immutable.SortedMap

/**Invariants over arrays can implement this trait to make it possible to bulk load their dependencies*/
trait Bulked[VarType <: Variable, BulkedComputationResult] extends Invariant{

  /**registers a static dependency to all variables mentioned in the bulkedVars.
   * @param bulkedVars: an iterable of variables to bulk together
   * @param id a reference name to identify to which bulk in the invariant this belongs to. Several bulks can be done with a single invariants
   * @param noBulk set to false if you want to bypass the bulk mechanism actually
   * @return the result of performBulkComputation(bulkedVars),  possibly computed by co-bulking invariants
   */
  final def bulkRegister(bulkedVars: Array[VarType], id:Int= 0, noBulk:Boolean = false): BulkedComputationResult = {

    if (noBulk){
      this.registerStaticDependencyAll(bulkedVars)
      return performBulkComputation(bulkedVars)
    }

    val m:Model = this.preFinishInitialization(bulkedVars(0).model)
    if (m == null){
      //no bulking possible
      this.registerStaticDependencyAll(bulkedVars)
      performBulkComputationID(bulkedVars,id)
    }else{
      //check for existing bulk
      val identifyingString = this.getClass.getName + "/" + id

      val incredibleBulk = m.getBulk(identifyingString, bulkedVars.asInstanceOf[Array[Variable]])

      if (incredibleBulk == null){
        //create a new bulk
        val bcr = performBulkComputation(bulkedVars)
        val newBulk = new Bulk(m, bulkedVars.asInstanceOf[Array[Variable]], bcr)
        this.registerStaticallyListenedElement(newBulk)
        m.registerBulk(identifyingString, newBulk)
        bcr
      }else{
        //we got it
        this.registerStaticallyListenedElement(incredibleBulk)
        
        incredibleBulk.bulkedComputationResult.asInstanceOf[BulkedComputationResult]
      }
    }
  }

  def performBulkComputationID(vars: Array[VarType], id:Int): BulkedComputationResult = performBulkComputation(vars)
  def performBulkComputation(vars: Array[VarType]): BulkedComputationResult = null.asInstanceOf[BulkedComputationResult]
}

/**This is the node that is put in the propagation graph
 * used by BulkLoad only*/
class Bulk(m:Model, val bulkedVars: Array[Variable], val bulkedComputationResult:Any)
  extends Invariant with BulkPropagator{

  for (dd <- bulkedVars) registerStaticallyListenedElement(dd)
  finishInitialization(m)
  override def getDotNode:String = "[label=\"Bulk\" shape=diamond]"
}

trait Bulker{

  var Bulked:SortedMap[String, List[Bulk]] = SortedMap.empty

  def getBulk(identifyingName:String, bulkedVars:Array[Variable]):Bulk = {
    val bulks = Bulked.getOrElse(identifyingName, null)
    
    if (bulks == null) return null
    
    for (b <- bulks){
      if (bulkedVars == b.bulkedVars){
        return b
      }
    }
    null
  }
  
  def registerBulk(identifyingName:String, bulk:Bulk){
    val knownbulk = Bulked.getOrElse(identifyingName, List.empty)
    Bulked += ((identifyingName, bulk :: knownbulk))
  }
}