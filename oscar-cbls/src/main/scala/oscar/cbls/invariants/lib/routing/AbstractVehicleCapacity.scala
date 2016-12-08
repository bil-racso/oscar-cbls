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

import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.{IntSequence, IntSequenceExplorer}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.routing.convention.{ConcreteVehicleLocation, VehicleLocation}

/**
 * Maintains the content of vehicles at each node and the starting position of each vehicle
 * @param n The maximum number of nodes
 * @param v The number of vehicles
 * @param op A function which describes the capacity between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
 */
abstract class AbstractVehicleCapacity(n:Int,
                                       v:Int,
                                       op:(Int,Int,Int)=>Int) extends Invariant{

  /**
   * we only require that zoneStart is <= list.head._2 if exists
   * @param zoneStart
   * @param zoneEnd
   * @param list
   * @return
   * @author renaud.delandtsheer@cetic.be
   */
  def smartPrepend(zoneStart: Int, zoneEnd:Int, list:List[(Int,Int)]): List[(Int,Int)] = {
    require(zoneStart <= zoneEnd)
    assert(list.sortWith((lft : (Int, Int), rgt : (Int, Int)) => lft._1 < rgt._1 && lft._2 < rgt._2).eq(list))
    list match {
      case Nil => (zoneStart, zoneEnd) :: list
      case (oldStart, oldEnd) :: tail =>
        require(zoneStart <= oldEnd)
        if (zoneEnd < oldStart - 1) {
          //the new interval does not touch the old one
          (zoneStart,zoneEnd) :: list
        } else {
          //the new interval touches the old one, there will be some merge
          smartPrepend(Math.min(zoneStart, oldStart), Math.max(zoneEnd, oldEnd), tail)
        }
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
          if (relativePosOfInsert < endZone) {
            smartPrepend(relativePosOfInsert, relativePosOfInsert+1, shiftPlusDelta(zonesToUpdate,1))
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


  // @Note => O(listToInsert+toInsert)
  private def insertInList(listToInsert: List[(Int, Int)], toInsert: List[(Int, Int)]): List[(Int, Int)] = {
    listToInsert match {
      case Nil => toInsert
      case (start, end) :: tail =>
        toInsert match {
          case Nil => listToInsert
          case (s, e) :: tail =>
            if (s > end) smartPrepend(start, end, insertInList(listToInsert.drop(1), toInsert))
            else if (s == start && end == e) insertInList(listToInsert, toInsert.drop(1))
            else smartPrepend(s, e, insertInList(listToInsert, toInsert.drop(1)))
        }
    }
  }

  def updateZoneToUpdateAfterMove(zonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                  m:SeqUpdateMove,
                                  sequenceBeforeMove:IntSequence,
                                  vehicleLocationBeforeMove:VehicleLocation):RedBlackTreeMap[List[(Int, Int)]] = ???


  def insertIntoToUpdateZone(fromPositionRelativeIncluded:Int,toPositionRelativeIncluded:Int,listOfZones:List[(Int,Int)]):List[(Int,Int)] = {
    listOfZones match{
      case Nil => List((fromPositionRelativeIncluded:Int,toPositionRelativeIncluded))
      case (start,end) :: tail =>
        if(end+1 < fromPositionRelativeIncluded){
          //the inserted zone does not touch (start,end), so recurse and prepend
          (start,end) :: insertIntoToUpdateZone(fromPositionRelativeIncluded,toPositionRelativeIncluded,tail)
        }else{
          //the inserted zone is before this one
          smartPrepend(fromPositionRelativeIncluded,toPositionRelativeIncluded,listOfZones)
        }
    }
  }

  def getVehicleContentAtNode(node: Int): Int

  /**
   * sets the conent of the vehicle at node "node"
   * if a node is unrouted, you should call setVehicleContentToUnroutedNode instead
   * @param node the node
   * @param newValueAtNode the new value for the content of the vehicle at this node
   */
  def setVehicleContentAtNode(node: Int, newValueAtNode: Int) : Unit

  def getContentAtVehicleStart(vehicle:Int): Int

  def setVehicleContentAtEnd(vehicle:Int,content:Int)

  def setEndNodeOfVehicle(vehicle:Int,lastNode:Int)

  def setNodesUnrouted(unroutedNodes:Iterable[Int])

  protected def updateVehicleContentOnAllVehicle(s:IntSequence,
                                                 vehiclesToZonesToUpdate: RedBlackTreeMap[List[(Int, Int)]],
                                                 vehicleLocationInSequence:VehicleLocation){

    var tmpExplorer:Option[IntSequenceExplorer] = None

    for((vehicle,sortedZonesToUpdateRelativeToVehicleStartPosition) <- vehiclesToZonesToUpdate.content){
      tmpExplorer = updateVehicleContent(s,
        sortedZonesToUpdateRelativeToVehicleStartPosition,
        vehicleLocationInSequence.startPosOfVehicle(vehicle),
        vehicle,
        tmpExplorer)

      tmpExplorer match{
        case Some(e) if e.position+1 == s.size || (e.position + 1 == vehicleLocationInSequence.startPosOfVehicle(vehicle+1)) =>
          setVehicleContentAtEnd(vehicle,getVehicleContentAtNode(e.value))
          setEndNodeOfVehicle(vehicle,e.value)
        case _ => ;
          //we did not reach th end of the vehicle route in the update, yet the last node might have hanged, so we have to update this
          val positionOfEndNodeOfVehicle = if(vehicle == v-1) s.size-1 else vehicleLocationInSequence.startPosOfVehicle(vehicle+1) -1
          val explorerAtEndNodeOfVehicleOpt = s.explorerAtPosition(positionOfEndNodeOfVehicle)
          val explorerAtEndNodeOfVehicle = explorerAtEndNodeOfVehicleOpt.get
          setEndNodeOfVehicle(vehicle,explorerAtEndNodeOfVehicle.value)
          setVehicleContentAtEnd(vehicle,getVehicleContentAtNode(explorerAtEndNodeOfVehicle.value))
          tmpExplorer = explorerAtEndNodeOfVehicleOpt
      }
    }
  }

  /**
   * performs the update of vehicle content on the mentioned vehicle
   * beware, this does not update the contentAtVehicleEnd; you must do it somewhere else.
   * @param s the sequence
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
              case _ => s.explorerAtPosition(positionToStartFromAbsolute).get
            }

            if (positionToStartFromAbsolute == startPositionOfVehicle) {
              //we need to update the value at vehicle start
              val newValueAtStart:Int = getContentAtVehicleStart(vehicle)
              val oldValueAtStart = getVehicleContentAtNode(positionToStartFromAbsolute)
              if(newValueAtStart != oldValueAtStart || endCompulsoryAbsolute > 0) {
                //start iterate from here
                setVehicleContentAtNode(positionToStartFromAbsolute, newValueAtStart)
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
            setVehicleContentAtNode(currentNode, newValueForCurrentNode)
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
  def computeAndAffectContentAndVehicleStartPositionsFromScratch(s:IntSequence,unrouteAllNodes:Boolean):(ConcreteVehicleLocation) = {
    val vehicleLocation = Array.fill(v)(0)

    if(unrouteAllNodes) setNodesUnrouted(0 until n)

    var previousPosition = s.explorerAtPosition(0).get
    var currentVehicle = 0
    var previousContent = getContentAtVehicleStart(0)
    setVehicleContentAtNode(0, previousContent)

    while(true) {
      previousPosition.next match {
        case None => //we'v reached the end of the sequence
          setVehicleContentAtEnd(currentVehicle,previousContent)
          setEndNodeOfVehicle(currentVehicle,previousPosition.value)
          require(currentVehicle == v-1)
          return VehicleLocation(vehicleLocation)
        case Some(currentPosition) =>
          val currentNode = currentPosition.value
          if (currentNode < v) {
            //we'v reached a new vehicle
            setVehicleContentAtEnd(currentVehicle,previousContent)
            setEndNodeOfVehicle(currentVehicle,previousPosition.value)
            vehicleLocation(currentNode) = currentPosition.position
            previousContent = getContentAtVehicleStart(currentNode)
            setVehicleContentAtNode(currentNode, previousContent)
            previousPosition = currentPosition
            currentVehicle = currentNode
          } else {
            //carry on the same vehicle
            //(startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
            previousContent = op(previousPosition.value, previousContent, currentNode)
            setVehicleContentAtNode(currentNode, previousContent)
            previousPosition = currentPosition
          }
      }
    }
    throw new Error("should not happen")
  }

  /**
   *Computes content of vehicle and their starting position from scratch
   * @param s the sequence
   * @return (nodeToContent,vehicleToContentAtEnd,vehicleLocation)
   */
  def computeNodeToContentAndVehicleContentAtEndAndVehicleStartPositionsFromScratch(s:IntSequence,defaultVehicleContentForUnroutedNodes:Int):(Array[Int],Array[Int],ConcreteVehicleLocation) = {
    val vehicleLocation = Array.fill(v)(0)
    val vehicleContent = Array.fill(n)(defaultVehicleContentForUnroutedNodes)
    val vehicleContentAtEndOfRoute = Array.fill(v)(0)

    var previousPosition = s.explorerAtPosition(0).get
    var currentVehicle = 0
    var previousContent = getContentAtVehicleStart(0)

    vehicleContent(0) = previousContent

    while(true) {
      previousPosition.next match {
        case None => //we'v reached the end of the sequence
          vehicleContentAtEndOfRoute(currentVehicle) = previousContent
          require(currentVehicle == v-1)
          return (vehicleContent,vehicleContentAtEndOfRoute,VehicleLocation(vehicleLocation))
        case Some(currentPosition) =>
          val currentNode = currentPosition.value
          if (currentNode < v) {
            //we'v reached a new vehicle
            vehicleContentAtEndOfRoute(currentVehicle) = previousContent
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
}
