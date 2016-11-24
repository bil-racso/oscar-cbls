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


package oscar.cbls.invariants.lib.routing
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._

/**
  * Created by  Jannou Brohée on 3/10/16.
  */

object GenericCumulativeIntegerDimensionOnVehicle {
  /**
    * Implements a GenericCumulativeIntegerDimensionOnVehicle Invariant
    *
    * @param routes The sequence representing the route associated at each vehicle
    * @param n the maximum number of nodes
    * @param v the number of vehicles
    * @param op a function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
    * @return the capacity of each node in the sequence representinf the route associeted at each vehicle
    */

  /**
    *
    * @param routes
    * @param n
    * @param v
    * @param op
    * @param initValue
    * @param maxStack
    * @return
    */
  def apply(routes:ChangingSeqValue,n:Int,v:Int,op :(Int,Int,Int)=>Int,initValue:Array[Int],minContent :Int=Int.MinValue, maxContent:Int=Int.MaxValue,maxStack:Int =4):Array[CBLSIntVar] ={
    var output: Array[CBLSIntVar] = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent), "capacity at node("+node.toString+")"))
    new GenericCumulativeIntegerDimensionOnVehicleInvariant(routes, n, v, op, initValue,output,maxStack).getOutput()
    output
  }
}


/**
  * Maintains the current capacity of each vehicle at each node after a SeqUpdate
  *
  * @param routes The sequence representing the route associated at each vehicle
  * @param n The maximum number of nodes
  * @param v The number of vehicles
  * @param op A function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
  * @param initValue An array giving the initial capacity of a vehicle at his starting node (0, v-1)
  * @param output The array which store, for any node, the capacity of the vehicle associated at the node
  * @param maxStack
  */
class GenericCumulativeIntegerDimensionOnVehicle(routes:ChangingSeqValue, n:Int, v:Int, op :(Int,Int,Int)=>Int, initValue :Array[Int], output:Array[CBLSIntVar],maxStack:Int)
  extends AbstractVehicleCapacity(routes,n,v,-1,initValue  ,op) with SeqNotificationTarget{

  private var contentAtNode : Array[Int]= Array.tabulate(n)((node:Int)=> -1)

  private var stack:VehicleLocation = ConcreteVehicleLocation(Array.tabulate(n)((node:Int)=> 0))

  require(initValue.length==v)
  require( output.length==n)

  computeAndAffectContentAndVehicleStartPositionsFromScratch()

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){
    val zoneToCompute = digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes)
    zoneToCompute match {
      case null => computeAndAffectContentAndVehicleStartPositionsFromScratch()
      case tree =>
        for(car <- tree.keys)  {
          val lst = zoneToCompute.get(car).get
          if (lst.nonEmpty) updateContentForSelectedZones(routes.newValue,lst,  positionOfVehicle(car)  ,car)
        }
    }
  }

  /**
    * Search the zones where changes occur following a SeqUpdate
    *
    * @param changes the SeqUpdate
    * @return a list that specify mandatory zones which must be computed. A zone is represented by his start and end position : (startPositionIncluded, endPositionIncluded). Note that the list is sorted by position ((x,y) <= (x',y') iff y <= x'  )
    */
  def digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate) : RedBlackTreeMap[List[(Int,Int)]] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastInsert(digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev),s,changes)
      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastRemove(digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev),r,changes)
      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastMove(digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev),m,changes)
      case SeqUpdateAssign(value : IntSequence) =>
        UpdateVehicleStartPositionsAndSearchZoneToUpdateAfterAssign(value)
      case SeqUpdateLastNotified(value:IntSequence) =>
        updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastNotified(value )
      case s@SeqUpdateDefineCheckpoint(prev:SeqUpdate,_) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev)
      case u@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(u.howToRollBack)
    }
  }

  override def setVehicleContentAtNode(currentNode: Int,currentPosition: Int, valueOfCurrentNode: Int): Unit = contentAtNode(currentNode) =valueOfCurrentNode

  override def getVehicleContentAtNode(nodeId: Int): Int =   contentAtNode(nodeId)

  override def positionOfVehicle(vehicle: Int): Int = stack.posOfVehicle(vehicle)

  override def vehicleReachingPosition(position: Int): Int = stack.vehicleReachingPosition(position)

  override def pushOnTopOfStack(oldToNewfunction: (Int) => Option[Int]): Unit ={
    stack = stack.push(oldToNewfunction)
    if(stack.depth>maxStack) stack = stack.regularize()
  }


  def computeAndAffectContentAndVehicleStartPositionsFromScratch(): Unit ={
    var tmp = computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
    stack = tmp._2
    contentAtNode = tmp._1
  }

  def getOutput(){
    for(node <- 0 until n) {
      output(node) := getVehicleContentAtNode(node)
    }
  }
}




class GenericCumulativeIntegerDimensionOnVehicleInvariant(routes:ChangingSeqValue, n:Int, v:Int, op :(Int,Int,Int)=>Int, initValue :Array[Int], output:Array[CBLSIntVar],maxStack:Int)
  extends GenericCumulativeIntegerDimensionOnVehicle(routes, n, v, op, initValue,output,maxStack) {

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- output) i.setDefiningInvariant(this)

 // override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate): Unit = super.notifySeqChanges(v, d, changes)
}