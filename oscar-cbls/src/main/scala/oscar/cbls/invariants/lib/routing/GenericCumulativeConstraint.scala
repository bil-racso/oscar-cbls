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

import oscar.cbls.algo.magicArray.{MagicBoolArray, MagicIntArray}
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{Checker, ErrorChecker}
/**
  * Created by  Jannou Brohée on 17/10/16.
  */

object GenericCumulativeConstraint {
  /**
    *
    * @param routes The sequence representing the route associated at each vehicle
    * @param n The maximum number of nodes
    * @param v The number of vehicles
    * @param op A function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
    * @param cMax
    * @param initContent
    * @param initValue An array giving the initial capacity of a vehicle at his starting node (0, v-1)
    * @param initVehiclePosition
    * @param globalViolation
    * @param vehicleViolation
    * @param maxStack
    * @return
    */
  def apply(routes:ChangingSeqValue,n:Int,v:Int,op :(Int,Int,Int)=>Int,cMax:Int,
            initValue:Array[Int],  initContent: Int=0 , minContent :Int=Int.MinValue, maxContent:Int=Int.MaxValue, maxStack:Int=4):(CBLSIntVar,Array[Int]) ={
    var violPerV: Array[Int] = Array.tabulate(v)((car: Int) => 0)
    var globalViol:CBLSIntVar=  CBLSIntVar(routes.model,0,Domain.coupleToDomain(minContent,maxContent),"Global Violation of Capacity")
    new GenericCumulativeConstraint(routes,n,v,op,cMax,initContent ,initValue,globalViol,violPerV,maxStack).getOutput()
    (globalViol,violPerV)

  }
}

/**
  * Implements a GenericCumulativeConstraint Invariant
  * Computes the exceedation of the threshold by the op function for a vehicle at a node ( done for each node of each vehicle)
  * @param routes The sequence representing the route associated at each vehicle
  * @param n The maximum number of nodes
  * @param v The number of vehicles
  * @param op A function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
  * @param cMax
  * @param initContent
  * @param initValue An array giving the initial capacity of a vehicle at his starting node (0, v-1)
  * @param initVehiclePosition
  * @param globalViolation
  * @param vehicleViolation
  * @param maxStack
  */
class GenericCumulativeConstraint(routes:ChangingSeqValue, n:Int, v:Int, op :(Int,Int,Int)=>Int, cMax:Int, initContent: Int ,initValue :Array[Int],
                                  globalViolation: CBLSIntVar, vehicleViolation:Array[Int], maxStack:Int)
  extends AbstractVehicleCapacity(routes,n,v,initContent,initValue  ,op)  with SeqNotificationTarget {

  private var contentAtNode : MagicIntArray= null
  private val vehiclesViolation : MagicIntArray= MagicIntArray(v, 0,vehicleViolation)
  private var changedGlobalViolationSinceCheckpoint : Boolean = false
  private var changedGlobalViolation:Int=  0
  private var stack:VehicleLocation = null
  private var stackAtCheckPoint:VehicleLocation = null

  require(initValue.length==v)
  require( vehicleViolation.length==v)
  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  globalViolation.setDefiningInvariant(this)

  computeAndAffectContentAndVehicleStartPositionsFromScratch()

  private def getOutput() = {
    globalViolation := GlobalViolation
    for(car <- 0 until v ) vehicleViolation(car) = vehiclesViolation(car)
  }

  private def computeAndAffectContentAndVehicleStartPositionsFromScratch(): Unit ={
    var tmp = computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
    stack = tmp._2
    contentAtNode = MagicIntArray(n, initContent,tmp._1)
    var currentCar = 0
    var valueOfCurrentNode = initContent
    var currentNode = routes.newValue.explorerAtPosition(0)
    while(currentNode.nonEmpty){
      if(currentNode.get.value < v ){
        currentCar = currentNode.get.value
        vehiclesViolation(currentCar) = 0
      }
      valueOfCurrentNode = contentAtNode(currentNode.get.value)
      vehiclesViolation(currentCar) = vehiclesViolation(currentCar)+  (if (valueOfCurrentNode > cMax) valueOfCurrentNode-cMax else if (valueOfCurrentNode < 0)  Math.abs(valueOfCurrentNode) else 0)
      currentNode = currentNode.get.next
    }
    globalViolation := vehicleViolation.sum
  }



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
  private def digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate) : RedBlackTreeMap[List[(Int,Int)]] = {
    changes match {
      case s@SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastInsert(digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev),s,changes)
      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) => {
        val car = stack.vehicleReachingPosition(pos)
        val toReturn = updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastRemove(digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev),r,changes)
        val capa = contentAtNode(r.removedValue)
        val violationToRemove = if(capa>cMax) capa-cMax else if(capa<0) math.abs(capa) else 0
        vehiclesViolation(car) = vehiclesViolation(car)-violationToRemove
        recordChangeOfGlobalViolation(GlobalViolation-violationToRemove)
        contentAtNode(r.removedValue) = initContent
        toReturn
      }
      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) => {
        val toReturn =  updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastMove(digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev),m,changes)
        var node = prev.newValue.explorerAtPosition(fromIncluded).get
        var work = true
        while(work){
          val newVehicle = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(changes.newValue,m.oldPosToNewPos(node.position).get)
          val oldVehcile = RoutingConventionMethods.cachedVehicleReachingPosition(routes.newValue,v)(prev.newValue,node.position)
          val capOfNode = contentAtNode(node.value)
          val violationAtNode = if (capOfNode>cMax) capOfNode-cMax else if (capOfNode<0) Math.abs(capOfNode) else 0
          if( newVehicle != oldVehcile)  {
            vehiclesViolation(oldVehcile)  = vehiclesViolation(oldVehcile)-violationAtNode
            vehiclesViolation(newVehicle) = vehiclesViolation(newVehicle)+ violationAtNode
          }
          work = node.position<toIncluded
          if(work) node = node.next.get
        }
        node=null
        toReturn
      }
      case SeqUpdateAssign(value : IntSequence) =>
        UpdateVehicleStartPositionsAndSearchZoneToUpdateAfterAssign(value)
      case SeqUpdateLastNotified(value:IntSequence) =>
        updateVehicleStartPositionsAndSearchZoneToUpdateAfterLastNotified(value )
      case s@SeqUpdateDefineCheckpoint(prev:SeqUpdate,_) =>
        val toReturn = digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev)
        saveCheckPoint()
        toReturn
      case u@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>
        recovery()
        RedBlackTreeMap.empty[List[(Int,Int)]]
    }
  }

  override def setVehicleContentAtNode(currentNode: Int,currentPosition:Int, valueOfCurrentNode: Int): Unit = {
    val oldContentOfNode= contentAtNode(currentNode)
    contentAtNode(currentNode) =valueOfCurrentNode
    computeViolationOfNode(currentNode,oldContentOfNode ,vehicleReachingPosition(currentPosition) )
  }

  override def getVehicleContentAtNode(nodeId: Int): Int =  contentAtNode(nodeId)

  override def positionOfVehicle(vehicle: Int): Int = stack.posOfVehicle(vehicle)

  override def vehicleReachingPosition(position: Int): Int = stack.vehicleReachingPosition(position)

  override def pushOnTopOfStack(oldToNewfunction: (Int) => Option[Int]): Unit ={
    stack = stack.push(oldToNewfunction)
    if(stack.depth>maxStack) stack = stack.regularize()
  }

  override def UpdateVehicleStartPositionsAndSearchZoneToUpdateAfterAssign(value: IntSequence): RedBlackTreeMap[List[(Int, Int)]] = {
    contentAtNode.drop()
    super.UpdateVehicleStartPositionsAndSearchZoneToUpdateAfterAssign(value)
  }

  /**
    * Saves the current context (capacity of each node and the global violation)
    *
    * @param _chk true to save false otherwise
    */
  private def saveCheckPoint(): Unit ={
    globalViolation:=GlobalViolation
    changedGlobalViolationSinceCheckpoint = false
    contentAtNode.save()
    vehiclesViolation.save()
    stackAtCheckPoint= stack.regularize()
  }



  /**
    * Recovers the saved context
    */
  private def recovery(): Unit ={
    changedGlobalViolationSinceCheckpoint = false
    vehiclesViolation.reload()
    contentAtNode.reload()
    stack = stackAtCheckPoint
    for(car <- 0 until v ) require(vehiclesViolation(car) ==  vehiclesViolation(car,false))
    for(no <- 0 until n ) require(contentAtNode(no) ==  contentAtNode(no,false))
  }

  /**
    * Returns the Global Violation of the current configuration of the nodes
    * @return violation
    */
  def GlobalViolation: Int = if(changedGlobalViolationSinceCheckpoint) changedGlobalViolation else globalViolation.newValue

  /**
    * Records the last global violation degree
    * @param value degree of global violation
    * @param save true if checkpoint false otherwise
    */
  private def recordChangeOfGlobalViolation(value:Int) {
    if(contentAtNode.saveChanges()) {
      changedGlobalViolationSinceCheckpoint= true
      changedGlobalViolation = value
    } else  globalViolation.:=(value)
  }




  /**
    * Compute the Global degree of violation after a update of the capacity of a node
    * @param currentNode the id of the node
    * @param oldValueOfCurrentNode the old capacity of the node
    */
  private def computeViolationOfNode(currentNode: Int, oldValueOfCurrentNode:Int, vehicle:Int ): Unit = {
    val currentCapacityOfNode = contentAtNode(currentNode)
    // old violation
    val oldViolationOfNode = if (oldValueOfCurrentNode > cMax)  oldValueOfCurrentNode - cMax
    else if (oldValueOfCurrentNode < 0)  Math.abs(oldValueOfCurrentNode) else 0
    // nw viol
    val currentViolation = if (currentCapacityOfNode > cMax) currentCapacityOfNode-  cMax  else if (currentCapacityOfNode < 0)  Math.abs(currentCapacityOfNode) else 0
    var oldViolation = vehiclesViolation(vehicle)
    vehiclesViolation(vehicle) = oldViolation+ currentViolation-oldViolationOfNode
    recordChangeOfGlobalViolation(GlobalViolation+(vehiclesViolation(vehicle)-oldViolation))
  }

  override def checkInternals(c: Checker): Unit = {
    val tmp = computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
    for(node <- routes.newValue) c.check(tmp._1(node) equals getVehicleContentAtNode(node), Some("GenericCumulativeConstraint : Founded Capacity at node(" + node + ") at pos : "+ routes.newValue.positionsOfValue(node)+ " :=" + getVehicleContentAtNode(node) + " should be :=" + tmp._1(node)))
    var currentCar = 0
    var valueOfCurrentNode = initContent
    var currentNode = routes.newValue.explorerAtPosition(0)
    var tmpViolPerV = Array.tabulate(v)((car :Int)=> 0)
    lazy val globalViolation = tmpViolPerV.sum
    while(currentNode.nonEmpty){
      if(currentNode.get.value < v ) currentCar = currentNode.get.value
      valueOfCurrentNode = tmp._1(currentNode.get.value)
      tmpViolPerV(currentCar) +=  (if (valueOfCurrentNode > cMax) valueOfCurrentNode-cMax else if (valueOfCurrentNode < 0)  Math.abs(valueOfCurrentNode) else 0)
      currentNode = currentNode.get.next
    }
    for(car <- 0 until v){
      c.check(tmp._2.posOfVehicle(car) equals positionOfVehicle(car ), Some("GenericCumulativeConstraint : Founded start of car(" + car + "):=" + positionOfVehicle(car) + " should be :=" + tmp._2.posOfVehicle(car)+" seq :"+routes.newValue.mkString(",")))
      c.check(vehiclesViolation(car) equals tmpViolPerV(car), Some("GenericCumulativeConstraint : Founded violation of vehicle("+car+") :="+vehiclesViolation(car)+" should be :="+tmpViolPerV(car)))
    }
    c.check(GlobalViolation equals globalViolation, Some("GenericCumulativeConstraint : Founded Violation :="+GlobalViolation+" should be :="+globalViolation))
  }
}

