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

import oscar.cbls.algo.magicArray.MagicBoolArray
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.{IntSequenceExplorer, IntSequence}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.{Checker, SchedulingHandler}
import oscar.cbls.invariants.lib.routing.convention.{ConcreteVehicleLocation, VehicleLocation}


/**
 * Created by  Jannou Brohée on 18/11/16.
 */


/**
 * Maintains the content of vehicles at each node and the starting position of each vehicle
 * @param routes The sequence representing the route associated at each vehicle
 * @param n The maximum number of nodes
 * @param v The number of vehicles
 * @param initInt The initial capacity of a node when it is not yet in the route
 * @param initValue An array giving the initial capacity of a vehicle at his starting node (0, v-1)
 * @param op A function which describes the capacity between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
 */
abstract class AbstractVehicleCapacity(routes:ChangingSeqValue, n:Int, v:Int, defaultVehicleContentForUnroutedNodes:Int, initValue:Array[Int], op :(Int,Int,Int)=>Int )
  extends Invariant()   {

  /**
   * @param zoneStart
   * @param zoneEnd
   * @param list
   * @return
   *        @author renaud.delandtsheer@cetic.be
   */
  def smartPrepend(zoneStart: Int, zoneEnd:Int, list:List[(Int,Int)]): List[(Int,Int)] ={
    require(zoneStart<=zoneEnd)
    assert(list.sortWith((lft:(Int,Int),rgt:(Int,Int))=> lft._1<rgt._1 && lft._2<rgt._2).eq(list))
    list match{
      case Nil => (zoneStart,zoneEnd) :: list
      case (a,b)::tail =>
        if(zoneEnd>=a-1 && zoneEnd<=b) (Math.min(zoneStart,a), Math.max(zoneEnd,b)) :: tail
        else if (b>=zoneStart && a <=zoneStart)smartPrepend(a, Math.max(zoneEnd,b), tail)
        else if(b<zoneStart ) throw new Error("not sorted :( b:"+b+"zoneStart:"+zoneStart)
        else (zoneStart,zoneEnd)::list
    }
  }

  private def shiftPlusDelta(l:List[(Int,Int)],delta:Int):List[(Int,Int)] = {
    l match {
      case Nil => l
      case (a, b) :: tail => (a + delta, b + delta) :: shiftPlusDelta(tail,delta)
    }
  }

  def updateZoneToUpdateAfterInsert(zoneToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                    posOfInsert:Int,
                                    sequenceBeforeInsert:IntSequence,
                                    vehicleLocationBeforeInsert:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {
    if (zoneToUpdate == null) return null
    val vehicleOfInsert = vehicleLocationBeforeInsert.vehicleReachingPosition(posOfInsert)
    val startPosOfVehicle = vehicleLocationBeforeInsert.startPosOfVehicle(vehicleOfInsert)
    val relativePosOfInsert = posOfInsert - startPosOfVehicle

    def addInsertionIntoZonesToUpdate(zonesToUpdate:List[(Int,Int)]):List[(Int,Int)] = {
      zonesToUpdate match {
        case Nil => List((relativePosOfInsert, relativePosOfInsert))
        case (startZone, endZone) :: tail =>
          if (relativePosOfInsert < startZone) {
            //we are before the startZone,endZone, and it cannot be incorporated into this zone
            //eg: if insert at relative pos 4 and zone starts at 5, the element at 4 is moved one upwards
            // and will separate the insert from the start
            (relativePosOfInsert, relativePosOfInsert) :: shiftPlusDelta(zonesToUpdate,1)
          } else if (relativePosOfInsert <= endZone + 1){
            //we are in the zone, or the zone can be extended to iclude the insertion point
            (startZone, endZone + 1) :: shiftPlusDelta(tail,1)
          } else {
            assert(relativePosOfInsert > endZone + 1)
            (startZone, endZone) :: addInsertionIntoZonesToUpdate(tail)
          }
      }
    }
    zoneToUpdate.insert(vehicleOfInsert, addInsertionIntoZonesToUpdate(zoneToUpdate.getOrElse(vehicleOfInsert, List.empty[(Int, Int)])))
  }


  def updateZoneToUpdateAfterRemove(zoneToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                    posOfRemove:Int,
                                    sequenceBeforeRemove:IntSequence,
                                    vehicleLocationBeforeRemove:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = {
    if (zoneToUpdate == null) return null
    val vehicleOfRemove = vehicleLocationBeforeRemove.vehicleReachingPosition(posOfRemove)
    val startPosOfVehicle = vehicleLocationBeforeRemove.startPosOfVehicle(vehicleOfRemove)
    val relativePosOfRemove = posOfRemove - startPosOfVehicle

    def addRemoveIntoZonesToUpdate(zonesToUpdate:List[(Int,Int)]):List[(Int,Int)] = {
      zonesToUpdate match {
        case Nil => List((relativePosOfRemove, relativePosOfRemove))
        case (startZone, endZone) :: tail =>
          if (relativePosOfRemove + 1 < startZone) {
            smartPrepend(relativePosOfRemove , relativePosOfRemove, shiftPlusDelta(zonesToUpdate,-1))
          } else if (relativePosOfRemove - 1 <= endZone){
            //we are in the zone, or the zone can be extended to include the insertion point
            (startZone, endZone - 1) :: shiftPlusDelta(tail,-1)
          } else {
            assert(vehicleOfRemove > endZone + 1)
            smartPrepend(startZone, endZone, addRemoveIntoZonesToUpdate(tail))
          }
      }
    }
    zoneToUpdate.insert(vehicleOfRemove, addRemoveIntoZonesToUpdate(zoneToUpdate.getOrElse(vehicleOfRemove, List.empty[(Int, Int)])))
  }





  /**
   * Returns the capacity associated with a node.
   * @param nodeId the id of the node
   * @param save false to return the capacity save at the previous checpoint, true to return the capacity calculated at the last movement
   * @return the capacity of the node
   */
  def getVehicleContentAtNode(node: Int): Int

  /**
   * Overridden the old capacity of a node by the new value.
   * @param currentNode the id of the node
   * @param valueOfCurrentNode the new capacity associated with the node
   * @param save false to override the current capacity, true to save the current capacity
   */
  def setVehicleContentAtNode2(node: Int, newValueAtNode:Int, vehicleOfNode:Int): Unit

  def getContentAtVehicleStart(vehicle:Int): Int = initValue(vehicle)

  def setVehiclecontentAtEnd(vehicle:Int,content:Int)

  protected def updateVehicleContentOnAllVehicle(s:IntSequence,
                                                 vehiclesToZonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                                 vehicleLocationInSequence:VehicleLocation){

    var tmpExmplorer:Option[IntSequenceExplorer] = None
    for((vehicle,sortedZonesToUpdateRelativeToVehicleStartPosition) <- vehiclesToZonesToUpdate.content){
      tmpExmplorer = updateVehicleContent(s,
        sortedZonesToUpdateRelativeToVehicleStartPosition,
        vehicleLocationInSequence.startPosOfVehicle(vehicle),
        vehicle,
        tmpExmplorer)

      tmpExmplorer match{
        case Some(e) if(e.position+1 == s.size || (e.position + 1 == vehicleLocationInSequence.startPosOfVehicle(vehicle+1))) =>
          setVehiclecontentAtEnd(vehicle,getVehicleContentAtNode(e.value))
        case _ => ;
      }
    }
  }

  /**
   * performs the update of vehicle content on the mentioned vehicle
   * beware, this does not update the contentAtVehicleEnd; you must do it somewhere else.
   * @param s the equence
   * @param sortedZonesToUpdateRelativeToVehicleStartPosition a sorted lsit of non-overlapping intervals wit hthe relative positions to update
   * @param startPositionOfVehicle the start position o he vehicle to update
   * @param vehicle the vehicle to update
   * @param explorerToLatestUpdatedPosition an explorer to the latest position that was updated
   * @return the last position that was updated
   */
  private def updateVehicleContent(s:IntSequence,
                                   sortedZonesToUpdateRelativeToVehicleStartPosition: List[(Int, Int)],
                                   startPositionOfVehicle:Int,
                                   vehicle:Int,
                                   explorerToLatestUpdatedPosition:Option[IntSequenceExplorer] = None):Option[IntSequenceExplorer] = {

    sortedZonesToUpdateRelativeToVehicleStartPosition match {
      case Nil => explorerToLatestUpdatedPosition
      case (startCompulsoryRelative, endCompulsoryRelative) :: tail =>
        val startCompulsoryAbsolute = startCompulsoryRelative + startPositionOfVehicle
        val endCompulsoryAbsolute = endCompulsoryRelative + startPositionOfVehicle

        val positionToStartFromAbsolute = explorerToLatestUpdatedPosition match {
          case Some(e) if e.position > startCompulsoryAbsolute => e.position
          case _ => startCompulsoryAbsolute
        }

        val explorerAfterUpdatingThisIntervalOpt =
          if (positionToStartFromAbsolute > endCompulsoryAbsolute) {
            explorerToLatestUpdatedPosition
          } else {
            val explorerToStartUpdate = explorerToLatestUpdatedPosition match {
              case Some(e) if e.position == startCompulsoryAbsolute => e
              case Some(e) if e.position == startCompulsoryAbsolute - 1 => e.next.get
              case _ => s.explorerAtAnyOccurrence(positionToStartFromAbsolute).get
            }

            if (positionToStartFromAbsolute == startPositionOfVehicle) {
              //we need to update the value at vehicle start
              val newValueAtStart:Int = getContentAtVehicleStart(vehicle)
              val oldValueAtStart = getVehicleContentAtNode(positionToStartFromAbsolute)
              if(newValueAtStart != oldValueAtStart || endCompulsoryAbsolute > 0) {
                //start iterate from here
                setVehicleContentAtNode2(positionToStartFromAbsolute, newValueAtStart, vehicle)
                updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(explorerToStartUpdate,
                  newValueAtStart,
                  endCompulsoryAbsolute, vehicle)
              }else {
                //we do not need to iterate on this because there was no change at this point, and we already reached the end of the compulsory zone
                Some(explorerToStartUpdate)
              }
            } else {
              //we start later than vehicle start
              //so we need to fetch the value at the previous node

              val explorerAtPrev = explorerToStartUpdate.prev.get
              updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(explorerAtPrev,
                getVehicleContentAtNode(explorerAtPrev.value),
                endCompulsoryAbsolute, vehicle)
            }
          }
        //and carry on to next interval
        updateVehicleContent(s,
          tail,
          startPositionOfVehicle,
          vehicle,
          explorerAfterUpdatingThisIntervalOpt)
    }
  }

  /**
   *
   * @param previousUpdatedPosition
   * @param valueAtPreviousUpdatedPosition
   * @param endCompulsoryAbsolute
   *                              @param vehicle the vehicle to which this belongs
   * @return the last position where an update was performed
   */
  private def updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(previousUpdatedPosition:IntSequenceExplorer,
                                                                  valueAtPreviousUpdatedPosition:Int,
                                                                  endCompulsoryAbsolute:Int,vehicle:Int):Option[IntSequenceExplorer] = {
    previousUpdatedPosition.next match{
      case None => None //we'v reached the end of the sequence
      case Some(positionOfCurrent) =>
        val currentNode = positionOfCurrent.value
        if(currentNode < v){
          //we'v just reached another vehicle start
          Some(previousUpdatedPosition)
        }else {
          //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
          val oldValueForCurrentNode = getVehicleContentAtNode(currentNode)
          val newValueForCurrentNode = op(previousUpdatedPosition.value, currentNode, valueAtPreviousUpdatedPosition)

          if (oldValueForCurrentNode != newValueForCurrentNode) {
            setVehicleContentAtNode2(currentNode, newValueForCurrentNode, vehicle)
            updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(
              positionOfCurrent,
              newValueForCurrentNode,
              endCompulsoryAbsolute, vehicle)
          } else if (positionOfCurrent.position < endCompulsoryAbsolute) {
            //thre is no update, but we are in the compulsory zone
            updateUntilAbsolutePositionAndSaturatedOrVehicelEnd(
              positionOfCurrent,
              newValueForCurrentNode,
              endCompulsoryAbsolute, vehicle)
          } else {
            //there is no update, and we are not in the compulsory zone anymore, so we can stop, and return the position
            Some(positionOfCurrent)
          }
        }
    }
  }

  /**
   *Computes content of vehicle and their starting position from scratch
   * @param s the sequence
   * @return (VehicleLocation)
   */
  def computeAndAffectContentAndVehicleStartPositionsFromScratch(s:IntSequence):(ConcreteVehicleLocation) = {
    val vehicleLocation = Array.fill(v)(0)

    var previousPosition = s.explorerAtPosition(0).get
    var currentVehicle = 0
    var previousContent = getContentAtVehicleStart(0)
    setVehicleContentAtNode2(0, previousContent , 0)

    while(true) {
      previousPosition.next match {
        case None => //we'v reached the end of the sequence
          setVehicleContentAtEnd(previousContent, currentVehicle)
          return VehicleLocation(vehicleLocation)
        case Some(currentPosition) =>
          val currentNode = currentPosition.value
          if (currentNode < v) {
            //we'v reached a new vehicle
            setVehicleContentAtEnd(previousContent, currentVehicle)
            vehicleLocation(currentNode) = currentPosition.position
            previousContent = getContentAtVehicleStart(currentNode)
            setVehicleContentAtNode2(currentNode, previousContent, currentVehicle)
            previousPosition = currentPosition
            currentVehicle = currentNode
          } else {
            //carry on the same vehicle
            //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
            previousContent = op(previousPosition.value, previousContent, currentNode)
            setVehicleContentAtNode2(currentNode, previousContent, currentVehicle)
            previousPosition = currentPosition
          }
      }
    }
    throw new Error("should not happen")
  }

  /**
   *Computes content of vehicle and their starting position from scratch
   * @param s the sequence
   * @return (VehicleLocation)
   */
  def computeAndReturnAndVehicleStartPositionsFromScratch(s:IntSequence):(Array[Int],ConcreteVehicleLocation) = {
    val vehicleLocation = Array.fill(v)(0)
    val vehicleContent = Array.fill(n)(defaultVehicleContentForUnroutedNodes)
    var previousPosition = s.explorerAtPosition(0).get
    var currentVehicle = 0
    var previousContent = getContentAtVehicleStart(0)
    setVehicleContentAtNode2(0, previousContent , 0)

    while(true) {
      previousPosition.next match {
        case None => //we'v reached the end of the sequence
          return (vehicleContent,VehicleLocation(vehicleLocation))
        case Some(currentPosition) =>
          val currentNode = currentPosition.value
          if (currentNode < v) {
            //we'v reached a new vehicle
            vehicleLocation(currentNode) = currentPosition.position
            previousContent = getContentAtVehicleStart(currentNode)
            vehicleContent(currentNode) = previousContent
            previousPosition = currentPosition
            currentVehicle = currentNode
          } else {
            //carry on the same vehicle
            //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
            previousContent = op(previousPosition.value, previousContent, currentNode)
            vehicleContent(currentNode) = previousContent
            previousPosition = currentPosition
          }
      }
    }
    throw new Error("should not happen")
  }


  /**
   * Update the stack of vehicles location
   * @param oldToNewfunction  a function representing the change of vehicles location
   */
  def pushOnTopOfStack(oldToNewfunction:(Int)=> Option[Int]) :Unit



  override def checkInternals(c: Checker): Unit = {
    val (capaToChekc, stackToChekc) = computeContentAndVehicleStartPositionsFromScratch(routes.newValue)
    for(node <- 0 until n) c.check(capaToChekc(node) equals getVehicleContentAtNode(node), Some("Founded Capacity at node(" + node + ") at pos : "+ routes.newValue.positionsOfValue(node)+ " :=" + getVehicleContentAtNode(node) + " should be :=" + capaToChekc(node)))
    for(car <- 0 until v)c.check(stackToChekc.startPosOfVehicle(car) equals positionOfVehicle(car ), Some("Founded start of car(" + car + "):=" + positionOfVehicle(car) + " should be :=" + stackToChekc.startPosOfVehicle(car)+" seq :"+routes.newValue.mkString(",")))
  }
}

