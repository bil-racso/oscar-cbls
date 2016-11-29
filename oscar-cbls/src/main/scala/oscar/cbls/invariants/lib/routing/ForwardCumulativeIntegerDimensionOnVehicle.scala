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
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.routing.convention.{ConcreteVehicleLocation, VehicleLocation}

/**
 * Created by  Jannou Brohée on 3/10/16.
 */

object ForwardCumulativeIntegerDimensionOnVehicle {
  /**
   * Implements a GenericCumulativeIntegerDimensionOnVehicle Invariant
   * @param routes The sequence representing the route associated at each vehicle
   * @param n The maximum number of nodes
   * @param v The number of vehicles
   * @param op A function which returns the capacity change between two nodes : (startingNode,destinationNode,capacityAtStartingNode)=> capacityAtDestinationNode
   * @param initValue Array of lenght = v where initValue(car) = content at start pos of vehicle #car
   * @param minContent Min content of a node
   * @param maxContent Max content of a node
   * @param maxStack Maximum depth of vehicleLocation history
   * @return The capacity of each node in the sequence representing the route associated at each vehicle
   */
  def apply(routes:ChangingSeqValue,
            n:Int,
            v:Int,
            op:(Int,Int,Int)=>Int,
            contentAtStart:Array[CBLSIntVar],
            defaultForUnroutedNodes:Int,
            minContent:Int = 0,
            maxContent:Int = Int.MaxValue,
            maxStack:Int = 4,
            contentName:String = "content"):(Array[CBLSIntVar],Array[CBLSIntVar],Array[CBLSIntVar]) ={
    val contentAtNode = Array.tabulate(n)((node: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent).union(defaultForUnroutedNodes), contentName + " at node "+node))
    val contentAtEnd = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, Domain.coupleToDomain(minContent,maxContent), contentName + " at end of route " + vehicle))
    val lastPointOfVehicle = Array.tabulate(v)((vehicle: Int) => CBLSIntVar(routes.model, 0, n-1, "last point of vehicle" + vehicle))

    new ForwardCumulativeIntegerDimensionOnVehicle(routes,n,v,op,contentAtStart,contentAtNode,contentAtEnd,lastPointOfVehicle,defaultForUnroutedNodes,maxStack)
    (contentAtNode,contentAtEnd,lastPointOfVehicle)
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
class ForwardCumulativeIntegerDimensionOnVehicle(routes:ChangingSeqValue,
                                                 n:Int,
                                                 v:Int,
                                                 op:(Int,Int,Int)=>Int,
                                                 contentAtStart:Array[CBLSIntVar],
                                                 contentAtNode:Array[CBLSIntVar],
                                                 contentAtEnd:Array[CBLSIntVar],
                                                 lastPointOfVehicle:Array[CBLSIntVar],
                                                 defaultForUnroutedNodes:Int,
                                                 maxStack:Int = 4)
  extends AbstractVehicleCapacity(routes,n,v,defaultForUnroutedNodes,op)
   with SeqNotificationTarget with IntNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  registerStaticAndDynamicDependencyArrayIndex(contentAtStart)
  finishInitialization()
  for(i <- contentAtNode) i.setDefiningInvariant(this)
  for(i <- contentAtEnd) i.setDefiningInvariant(this)

  //the output is initialized here, together with the currentVehicleLocation
  private var currentVehicleLocation:VehicleLocation = computeAndAffectContentAndVehicleStartPositionsFromScratch(routes.value)

  private val vehicleLocationAndCheckpointStack = new SeqCheckpointedValueStack[VehicleLocation]()

  private var toUpdateZonesAndVehiceStartAfter:Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)] = Some(RedBlackTreeMap.empty[List[(Int,Int)]],currentVehicleLocation)
  private var potentiallyRemovedNodes:List[Int] = List.empty

  override def getVehicleContentAtNode(node : Int) : Int = contentAtNode(node).newValue

  override def setEndNodeOfVehicle(vehicle : Int, lastNode : Int) : Unit = lastPointOfVehicle(vehicle) := lastNode

  override def setVehicleContentAtEnd(vehicle : Int, content : Int) : Unit = contentAtEnd(vehicle) := content

  override def setVehicleContentAtNode(node : Int, newValueAtNode : Int, vehicleOfNode : Int) : Unit = contentAtNode(node) := newValueAtNode

  override def setVehicleContentToUnroutedNode(node : Int) : Unit = contentAtNode(node) := defaultForUnroutedNodes

  override def getContentAtVehicleStart(vehicle : Int) : Int = contentAtStart(vehicle).value

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate){
    val tmp = digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes,toUpdateZonesAndVehiceStartAfter,potentiallyRemovedNodes,v.value)
    toUpdateZonesAndVehiceStartAfter = tmp._1
    potentiallyRemovedNodes = tmp._2
    scheduleForPropagation()
  }

  override def notifyIntChanged(v: ChangingIntValue, id: Int, OldVal: Int, NewVal: Int): Unit ={
    toUpdateZonesAndVehiceStartAfter match {
      case None => ;
      case Some((toUpdateZones,vehicleLocation)) =>
        toUpdateZonesAndVehiceStartAfter = Some((toUpdateZones.insert(id, smartPrepend(0,0,toUpdateZones.getOrElse(id,List.empty[(Int,Int)]))),vehicleLocation))
    }
    scheduleForPropagation()
  }

  override def performPropagation(): Unit = {
    updateForUnroutedNodes(potentiallyRemovedNodes)
    toUpdateZonesAndVehiceStartAfter match{
      case Some((vehiclesToZonesToUpdate,vehicleLocation)) =>
        updateVehicleContentOnAllVehicle(routes.value,
          vehiclesToZonesToUpdate,
          vehicleLocation)
        currentVehicleLocation = vehicleLocation
      case None =>
        currentVehicleLocation = computeAndAffectContentAndVehicleStartPositionsFromScratch(routes.value)
    }
    toUpdateZonesAndVehiceStartAfter = Some(RedBlackTreeMap.empty[List[(Int,Int)]],currentVehicleLocation)
    potentiallyRemovedNodes = List.empty
  }

  /**
   * Search the zones where changes occur following a SeqUpdate
   * @param changes the SeqUpdate
   * @return RedBlackTreeMap[List[(Int,Int)]] of sorted list of pos for vehicle
   */
  def digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(changes:SeqUpdate,
                                                                       toUpdateZonesAndVehiceStartOpt:Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)],
                                                                       potentiallyRemovedPoints:List[Int],
                                                                       previousSequence:IntSequence)
  :(Option[(RedBlackTreeMap[List[(Int,Int)]],VehicleLocation)],List[Int]) = {
    changes match {
      case s@SeqUpdateInsert(value : Int, posOfInsert : Int, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val updatedZones =
              updateZoneToUpdateAfterInsert(
                zonesAfterPrev,
                posOfInsert,
                prev.newValue,
                vehicleLocationAfterPrev)
            (Some((updatedZones,  vehicleLocationAfterPrev.push(s.oldPosToNewPos))), potentiallyRemovedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case r@SeqUpdateRemove(pos : Int, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val updatedZones =
              updateZoneToUpdateAfterRemove(
                zonesAfterPrev,
                pos : Int,
                prev.newValue, vehicleLocationAfterPrev)
            (Some((updatedZones, vehicleLocationAfterPrev.push(r.oldPosToNewPos))), r.removedValue :: potentiallyRemovedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, r.removedValue :: potentiallyRemovedPointsAfterPrev)
        }

      case m@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), potentiallyRemovedPointsAfterPrev) =>
            val updatedZones =
              updateZoneToUpdateAfterMove(
                zonesAfterPrev,
                m,
                prev.newValue, vehicleLocationAfterPrev)
            (Some((updatedZones, vehicleLocationAfterPrev.push(m.oldPosToNewPos))), potentiallyRemovedPointsAfterPrev)
          case(None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case SeqUpdateAssign(value : IntSequence) =>
        (None, potentiallyRemovedPoints ::: previousSequence.unorderedContentNoDuplicate)

      case SeqUpdateLastNotified(value : IntSequence) =>
        (toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints)

      case s@SeqUpdateDefineCheckpoint(prev : SeqUpdate, isStarMode:Boolean, checkpointLevel:Int) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(prev, toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          //checkpoints are managed about the vehicleLocation exclusively
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), removedPointsAfterPrev) =>
            val regularizedVehicleLocation = vehicleLocationAfterPrev.regularize
            vehicleLocationAndCheckpointStack.defineCheckpoint(prev.newValue,checkpointLevel,regularizedVehicleLocation)
            (Some((zonesAfterPrev, regularizedVehicleLocation)), removedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            (None, potentiallyRemovedPointsAfterPrev)
        }

      case u@SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence, level:Int) =>
        digestUpdatesAndUpdateVehicleStartPositionsAndSearchZoneToUpdate(u.howToRollBack,toUpdateZonesAndVehiceStartOpt, potentiallyRemovedPoints, previousSequence) match {
          //checkpoints are managed about the vehicleLocation exclusively
          case (Some((zonesAfterPrev, vehicleLocationAfterPrev)), removedPointsAfterPrev) =>
            val regularizedVehicleLocation = vehicleLocationAndCheckpointStack.rollBackAndOutputValue(checkpoint,level)
            (Some((zonesAfterPrev, regularizedVehicleLocation)), removedPointsAfterPrev)
          case (None,potentiallyRemovedPointsAfterPrev) =>
            //in this case, we cannot exploit the regularized info
            //but this is a very strange case,actually
            vehicleLocationAndCheckpointStack.rollBackAndOutputValue(checkpoint,level)
            (None, potentiallyRemovedPointsAfterPrev)
        }
    }
  }
}