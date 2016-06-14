package oscar.cbls.invariants.lib.routing

/*
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

import oscar.cbls.invariants.core.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._

import scala.collection.immutable.{SortedMap, SortedSet}


class NodeVehicleRestrictionsNaive(routes:ChangingSeqValue,
                                   v:Int,
                                   nodeVehicleRestrictions:List[(Int,Int)], useSimplePrecomputation:Boolean) extends SetInvariant() with SeqNotificationTarget {

  val n = routes.maxValue+1

  //TODO: test this, and go for O(1) implem
  //this is a naive implem. the O51) implem is for next round and will use:
  //  array node n => SortedMap vehicle v => number of node from start of vehicle reaching n that cannot be reached by vehicle v

  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  private val nodeToVehiclesRestriction : SortedMap[Int, SortedSet[Int]] = {
    var current = SortedMap.empty[Int, SortedSet[Int]]
    for ((node, vehicle) <- nodeVehicleRestrictions) {
      val currentVehiclesForNode = current.getOrElse(node, SortedSet.empty[Int])
      current = current + ((node, currentVehiclesForNode + vehicle))
    }
    current
  }

  def isAllowed(node : Int, vehicle : Int) = {
    nodeToVehiclesRestriction.get(node) match {
      case None => true
      case Some(vehicles) => !vehicles.contains(vehicle)
    }
  }

  def isForbidden(node : Int, vehicle : Int) = {
    nodeToVehiclesRestriction.get(node) match {
      case None => false
      case Some(vehicles) => vehicles.contains(vehicle)
    }
  }

  def forbiddenVehicles(node:Int):Iterable[Int] =
    nodeToVehiclesRestriction.get(node) match {
    case None => None
    case Some(vehicles) => vehicles
  }

  var checkpoint : IntSequence = null
  var violatedNodesAtCheckpoint : SortedSet[Int] = SortedSet.empty

  val isPrecomputationValieForVehicle:Array[Boolean] = Array.fill(v)(false)

  //node n => vehicle v => number of node from start of vehicle reaching n that cannot be reached by vehicle v
  val precomputationAtCheckpoint:Array[Array[Int]] = if(useSimplePrecomputation){
    Array.tabulate(n)(_=>Array.fill(v)(0))
  }else null

  def updatePrecomputation(vehicle:Int,seq:IntSequence){
    var it =  seq.explorerAtAnyOccurrence(vehicle)

    var currentRestrictionCount:SortedMap[Int,Int] = SortedMap.empty[Int,Int]

    while(it match{
      case None => false
      case Some(position) =>
        for(forbiddenVehicle <- forbiddenVehicles(position.value)) {
          currentRestrictionCount = addRestrictionToRestrictionAccumulator(currentRestrictionCount, forbiddenVehicle)
        }
        //now set this as the cumulative restriction for this position
        precomputationAtCheckpoint(position.value) = currentRestrictionCount
    })
  }


  def addRestrictionToRestrictionAccumulator(restrictionAccumulator:SortedMap[Int,Int],vehicle:Int):SortedMap[Int,Int] = {
    restrictionAccumulator + ((vehicle,restrictionAccumulator.getOrElse(vehicle,0)))
  }

  def computeFromScratch(value : IntSequence) : SortedSet[Int] = {
    var violatedNodes = SortedSet.empty[Int]

    val it = value.iterator
    var currentVehicle : Int = it.next()

    require(currentVehicle == 0)

    while (it.hasNext) {
      val node = it.next()
      if (node < v) {
        //reaching a new vehicle start
        currentVehicle = node
      } else if (isForbidden(node, currentVehicle)) {
        violatedNodes = violatedNodes + node
      }
    }
    violatedNodes
  }

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) {
    if (!digestUpdates(changes, false)) {
      this := computeFromScratch(changes.newValue)
    }
  }

  def saveCheckpoint(value : IntSequence) {
    checkpoint = value
    violatedNodesAtCheckpoint = this.newValue
  }

  def restoreCheckpoint(value : IntSequence) {
    require(checkpoint quickEquals value)
    this := violatedNodesAtCheckpoint
  }

  private def digestUpdates(changes : SeqUpdate, skipNewCheckpoints : Boolean) : Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive : Boolean) =>
        if(skipNewCheckpoints) {
          digestUpdates(prev, true)
        }else{
          if (!digestUpdates(prev, true)) {
            this := computeFromScratch(prev.newValue)
          }
          saveCheckpoint(prev.newValue)
          true
        }
      case SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence) =>
        require(checkpoint quickEquals this.checkpoint)
        this := violatedNodesAtCheckpoint
        true
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if (!digestUpdates(prev, skipNewCheckpoints)) return false

        val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(pos, prev.newValue, v)
        if (isForbidden(value, vehicleOfMovedSegment)) {
          this :+= value
        }

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if (!digestUpdates(prev, skipNewCheckpoints)) false
        else if (x.isNop) true
        else if (x.isSimpleFlip) {
          //this is a simple flip, no change on vehicle violation, since obeys routing rule!
          true
        } else {
          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded, prev.newValue, v)
          val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after, prev.newValue, v)
          assert(vehicleOfMovedSegment == RoutingConventionMethods.searchVehicleReachingPosition(toIncluded, prev.newValue, v))

          if (vehicleOfMovedSegment != targetVehicleOfMove) {
            //the segment is moved to another vehicle
            for (node <- prev.newValue.valuesBetweenPositions(fromIncluded, toIncluded)) {
              if (isForbidden(node, targetVehicleOfMove)) {
                this :+= node //maybe it was already in.
              } else {
                this :-= node //maybe it was already out.
              }
            }
          }
          true
        }

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        if (!digestUpdates(prev, skipNewCheckpoints)) return false
        this :-= x.removedValue
        true

      case SeqUpdateLastNotified(value : IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateSet(value : IntSequence) =>
        false //impossible to go incremental
    }
  }
}
*/