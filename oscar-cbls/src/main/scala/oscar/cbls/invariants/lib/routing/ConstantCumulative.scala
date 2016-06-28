package oscar.cbls.invariants.lib.routing

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


import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._

class ConstantCumulative(routes:ChangingSeqValue,
                              v:Int,
                              cumulativeCost:Array[Array[Int]],
                              isCumulativeCostSymmetric:Boolean,
                              initLeave:Array[Int], //value upon leaving the vehicle
                              hardMaxAtNode:Array[Int], //for vehicle nodes, this is taken into account at the end, initLeave is in initLeave
                              hardMinAtNode:Array[Int],//for vehicle nodes, this is taken into account at the end, initLeave is in initLeave
                              maxSlack:Int, //slack tht can be added upon arrival at node to enforce hardMinAtNode
                              valueAtEnd:Array[CBLSIntVar], //that's the value upon entering the start node again (thus counting min at node, and slack if necessary)
                              violation:CBLSIntVar) //violation is the total of violated hardMax and hardMin
  extends Invariant() with SeqNotificationTarget{

  val vehicles = 0 until v
  val n = routes.maxValue + 1

  //assert(value_in <= hardMaxAtNode)
  //assert(value_in + maxSlack >= hardMinAtNode)
  //value_out = max(value_in,hardMinAtNode) + costAtNode

  registerStaticAndDynamicDependency(routes)
  this.finishInitialization()
  for(x <- valueAtEnd) x.setDefiningInvariant(this)
  violation.setDefiningInvariant(this)

  def computeFromScratch(seq:IntSequence){
    var viol = 0
    for(vehicle <- vehicles){

      var valueAtLeavePrev = initLeave(vehicle)
      var prevNode = vehicle
      var explorerInOpt = seq.explorerAtAnyOccurrence(vehicle).head.next

      while( explorerInOpt match{
        case None => //end of last vehicle
        case Some(explorer) =>
          val node = explorer.value


          val arrivalAtNode = valueAtLeavePrev + cumulativeCost(prevNode)(node)

          if (arrivalAtNode > hardMaxAtNode(node)) {
            //violation
            viol += (hardMaxAtNode(node) - arrivalAtNode)
          }

          if (arrivalAtNode + maxSlack < hardMinAtNode(node)) {
            //violation
            viol += (hardMinAtNode(node) - (arrivalAtNode + maxSlack))
          }

          val enteringNode = Math.max(arrivalAtNode, hardMinAtNode(node))

          valueAtLeavePrev = enteringNode + cumulativeCost(node)(node)
          prevNode = node
          explorerInOpt = explorer.next
          if(node < v) {
            //back at the vehicle, so setting the return value.
            valueAtEnd(vehicle) := valueAtLeavePrev
            false
          } else true

      }){} //end loop on vehicle "vehicle"

    } // end for vehicle

    violation := viol
  }

  val arrivalTimeAtCheckpointM1IUnrouted:Array[Int] = Array.fill(n)(0)

  //on maintient l'heure d'arrivée à chaque noeud?




}
