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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by De Landtsheer Renaud
  ******************************************************************************/

package oscar.cbls.routing.model

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.invariants.core.computation.CBLSIntVar.int2IntVar
import oscar.cbls.invariants.lib.numeric.SumElements
import oscar.cbls.modeling.Algebra.InstrumentArrayOfIntVar

/**
 * Maintains a integer weight on each node to help to form constraints (adding information).
 */
class NodeWeighting(vrp:VRP, weightingName:String = "weight"){

  /**
   * the data structure array which maintains weights.
   */
  val nodeWeight : Array[CBLSIntVar] = Array.tabulate(vrp.N)(i => CBLSIntVar(vrp.m, Int.MinValue, Int.MaxValue, 0,
    weightingName + "_" + i))
}

/** maintains a cost associated to each vehicle
  *the cost is the sum of the cost associated to each node crossed by the vehicle
  */
class CommutativeCapacity(vrp:VRP with NodesOfVehicle, CapacityName:String = "CumulativeCapacity") extends NodeWeighting(vrp,CapacityName){
  val CostOfVehicle = Array.tabulate(vrp.V)(v => SumElements(nodeWeight,vrp.NodesOfVehicle(v)).toIntVar)
}

/** the capacity out is computed from the capacity out of the predecessor
  *
  * @param vrp the routing model
  * @param CapacityOut the capacity when leaving a node
  */
class NonCommutativeProgressiveCapacity(vrp:VRP with Predecessors,
                                var CapacityOut:Array[CBLSIntVar] =  null){

  if(CapacityOut  == null)
    CapacityOut = Array.tabulate(vrp.N)(n => CBLSIntVar(vrp.m,name = "CapacityOutOf_" + n))

  /** the capacity when leaving the predecessor of a given node */
  val CapacityOutPred:Array[CBLSIntVar] = Array.tabulate(vrp.N)(n => if (n < vrp.V) 0 else CapacityOut.element(vrp.preds(n)))
}

