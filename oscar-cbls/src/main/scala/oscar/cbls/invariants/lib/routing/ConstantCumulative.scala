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


import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

class ConstantCumulative(routes:ChangingSeqValue,
                         v:Int,
                         cumulativeCost:Array[Array[Int]],
                         isCumulativeCostSymmetric:Boolean,
                         hardMaxAtNode:Array[Int],
                         slippingMinAtNode:Array[Int],
                         maxIncreaseBeforeNode:Int)
  extends Invariant() with SeqNotificationTarget{

  val perVehicle:Boolean = distance.length >1
  require(distance.length == 1 || distance.length == v)

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(i <- distance) i.setDefiningInvariant(this)


  //TODO: improve pre-computation for non-symetric and multi-vehicle distance!

  //TODO: handle inactive checkpoints
  private val savedValues:Array[Int] = computeValueFromScratch(routes.value)
  private var savedCheckpoint = routes.value
  private val touchedRoutesSinceCheckpointArray:Array[Boolean] = Array.fill(v)(false)
  private var touchedRoutesSinceCheckpointList:QList[Int] = null

  affect(savedValues)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    if(!digestUpdates(changes,false)) {
      for(v <- 0 until this.v) recordTouchedVehicle(v)
      affect(computeValueFromScratch(changes.newValue))
    }
  }

  private def digestUpdates(changes:SeqUpdate,skipNewCheckpoints:Boolean):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive:Boolean) =>
        if(!digestUpdates(prev,true)){
          affect(computeValueFromScratch(changes.newValue))
        }
        saveCurrentCheckpoint(changes.newValue)
        true
      case SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>
        require (checkpoint quickEquals savedCheckpoint)
        restoreCheckpoint()
        true
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev,skipNewCheckpoints)) return false
        val newSeq = changes.newValue

        val oldPrev = prev.newValue.valueAtPosition(pos-1).head

        val oldSucc =prev.newValue.valueAtPosition(pos) match{
          case None => v-1 //at the end
          case Some(oldSuccIfNoLoop) =>  if(oldSuccIfNoLoop < v) oldSuccIfNoLoop-1 else oldSuccIfNoLoop
        }

        val oldDistance = distanceMatrix(oldPrev)(oldSucc)
        val newDistance = distanceMatrix(oldPrev)(value) + distanceMatrix(value)(oldSucc)
        val nodeCost = distanceMatrix(value)(value)

        if(perVehicle) {
          val vehicle = RoutingConventionMethods.searchVehicleReachingPosition(pos, newSeq, v)
          recordTouchedVehicle(vehicle)
          distance(vehicle) :+= (newDistance + nodeCost - oldDistance)
        }else{
          distance(0) :+= (newDistance + nodeCost - oldDistance)
        }
        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev,skipNewCheckpoints)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          //this is a simple flip

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToValue = RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v)

          val fromValue = x.fromValue
          val toValue = x.toValue

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val newHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(toValue)
          val newHopAfterMovedSegment = distanceMatrix(fromValue)(oldSuccToValue)

          val deltaDistance = if(distanceIsSymmetric) 0 else {
            //there is a flip and distance is asymmetric
            computeValueBetween(x.newValue, x.oldPosToNewPos(toIncluded).head,x.oldPosToNewPos(fromIncluded).head) - computeValueBetween(prev.newValue, fromIncluded, toIncluded)
          }

          //for simple flip, there is no node cost to consider
          if(perVehicle) {
            val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded, prev.newValue, v)
            recordTouchedVehicle(vehicleOfMovedSegment)
            distance(vehicleOfMovedSegment) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
              - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment) + deltaDistance)
          }else{//not per vehicle
            distance(0) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
              - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment) + deltaDistance)
          }
          true
        }else {
          //actually moving, not simple flip
          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToIfNoLoopOpt = prev.newValue.valueAtPosition(toIncluded + 1)
          val oldSuccToValue = oldSuccToIfNoLoopOpt match {
            case None => v - 1
            case Some(value) => if (value < v) value - 1 else value
          }

          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldSuccAfterValue = RoutingConventionMethods.routingSuccPos2Val(after, prev.newValue, v)

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val oldHopAfterAfter = distanceMatrix(afterValue)(oldSuccAfterValue)

          val newHopBeforeMovedSegment = distanceMatrix(afterValue)(if(flip) toValue else fromValue)
          val newHopAfterMovedSegment = distanceMatrix(if(flip) fromValue else toValue)(oldSuccAfterValue)
          val newHopReplacingSegment = distanceMatrix(oldPrevFromValue)(oldSuccToValue)

          if(!perVehicle){
            //not per vehicle, so no node cost to consider
            val deltaDistance = if(distanceIsSymmetric || !flip) 0 //no delta on distance
            else computeValueBetween(x.newValue, x.oldPosToNewPos(toIncluded).head,x.oldPosToNewPos(fromIncluded).head) - computeValueBetween(prev.newValue, fromIncluded, toIncluded)
            //there is a flip and distance is asymmetric

            distance(0) :+= (
              newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

          }else {
            //per vehicle, there might be some node cost to consider
            val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded, prev.newValue, v)
            val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after, prev.newValue, v)
            assert(vehicleOfMovedSegment == RoutingConventionMethods.searchVehicleReachingPosition(toIncluded, prev.newValue,v))

            if (vehicleOfMovedSegment == targetVehicleOfMove) {
              //the segment is moved to the same vehicle, so we do not consider node cost here

              val (deltaDistance) = if(distanceIsSymmetric || !flip) 0 else {
                //there is a flip and distance is asymmetric
                computeValueBetween(x.newValue, x.oldPosToNewPos(toIncluded).head,x.oldPosToNewPos(fromIncluded).head) - computeValueBetween(prev.newValue, fromIncluded, toIncluded)
              }

              recordTouchedVehicle(vehicleOfMovedSegment)
              distance(vehicleOfMovedSegment) :+= (
                newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                  - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

            } else {
              //moving a segment to another vehicle, and per vehicle required.

              //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
              val oldCostInSegment = computeValueBetween(prev.newValue, fromIncluded, toIncluded,true)
              val newCostInSegment = if(distanceIsSymmetric || !flip) oldCostInSegment else{
                //there is a flip and distance is asymmetric
                computeValueBetween(x.newValue, x.oldPosToNewPos(toIncluded).head,x.oldPosToNewPos(fromIncluded).head,true)
              }

              recordTouchedVehicle(vehicleOfMovedSegment)
              distance(vehicleOfMovedSegment) :+= (
                newHopReplacingSegment - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldCostInSegment))

              recordTouchedVehicle(targetVehicleOfMove)
              distance(targetVehicleOfMove) :+= (
                newHopBeforeMovedSegment + newCostInSegment + newHopAfterMovedSegment - oldHopAfterAfter)
            }
          }
          true
        }

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        val removedValue = x.removedValue
        //node cost to be considered
        if(!digestUpdates(prev,skipNewCheckpoints)) return false

        val positionOfDelete = x.position

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1).head //vehicles are never deleted

        val oldSuccValue = RoutingConventionMethods.routingSuccPos2Val(positionOfDelete-1, prev.newValue,v)

        val newDistance = distanceMatrix(oldPrevValue)(oldSuccValue)
        val oldDistanceBefore = distanceMatrix(oldPrevValue)(removedValue)
        val oldDistanceAfter = distanceMatrix(removedValue)(oldSuccValue)
        val nodeCost = distanceMatrix(removedValue)(removedValue)

        if(perVehicle){
          val vehicle = RoutingConventionMethods.searchVehicleReachingPosition(positionOfDelete, prev.newValue,v)
          recordTouchedVehicle(vehicle)
          distance(vehicle) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter + nodeCost))
        }else{
          distance(0) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter + nodeCost))
        }
        true

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        true //we are starting from the previous value
      case SeqUpdateSet(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  private def saveCurrentCheckpoint(s:IntSequence){
    savedCheckpoint = s
    if(perVehicle) {
      while (touchedRoutesSinceCheckpointList != null) {
        touchedRoutesSinceCheckpointArray(touchedRoutesSinceCheckpointList.head) = false
        touchedRoutesSinceCheckpointList = touchedRoutesSinceCheckpointList.tail
      }
    }else{
      savedValues(0) = distance(0).newValue
    }
  }

  private def restoreCheckpoint(){
    if(perVehicle) {
      while (touchedRoutesSinceCheckpointList != null) {
        val v = touchedRoutesSinceCheckpointList.head
        distance(v) := savedValues(v)
        touchedRoutesSinceCheckpointArray(v) = false
        touchedRoutesSinceCheckpointList = touchedRoutesSinceCheckpointList.tail
      }
    }else{
      distance(0) := savedValues(0)
    }
  }

  private def recordTouchedVehicle(v:Int){
    if(perVehicle){
      if(savedCheckpoint!= null && !touchedRoutesSinceCheckpointArray(v)){
        savedValues(v) = distance(v).value
        touchedRoutesSinceCheckpointArray(v) = true
        touchedRoutesSinceCheckpointList = QList(v,touchedRoutesSinceCheckpointList)
      }
    }
  }

  private def affect(value:Array[Int]){
    var currentV = distance.length
    while(currentV >0){
      currentV -= 1
      distance(currentV) := value(currentV)
    }
  }

  //TODO: there is a O(1) way
  // labeled forward and backward nodes with their cumulated distance
  // use invalidation per vehicle in case more than one move is performed
  // just one thing: backtrack is only performed through checkpoint; star mode will lead to recomputation of the vehicles from scratch
  //datastruct for checkpoint: forward et bw labeling per vehicle. labeling: node -> (forward,backward) in a redBlack
  private def computeValueBetween(s:IntSequence, fromPosIncluded:Int, toPosIncluded:Int,addNodeCost:Boolean = false):Int = {
    assert(fromPosIncluded <= toPosIncluded)

    var e = s.explorerAtPosition(fromPosIncluded).head
    var toReturn = 0
    if(addNodeCost) toReturn +=  distanceMatrix(e.value)(e.value)

    while(e.position < toPosIncluded){
      val nextPos = e.next.head
      toReturn += distanceMatrix(e.value)(nextPos.value)
      if(addNodeCost) toReturn += distanceMatrix(nextPos.value)(nextPos.value)
      e = nextPos
    }
    toReturn
  }

  private def computeValueFromScratch(s:IntSequence):Array[Int] = {
    val toReturn = Array.tabulate(v)(v => distanceMatrix(v)(v))
    val it = s.iterator

    var prevNode:Int = it.next()
    var currentVehicle:Int = prevNode

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        //finishing the circle (cost of vehicle node already added)
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(currentVehicle)
        currentVehicle = node
      }else{
        //continuing on the same vehicle
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(node) +  distanceMatrix(node)(node)
      }
      prevNode = node
    }
    //for the last vehicle, the finishing operation in the loop will not be executed, so we have to add one here
    toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(currentVehicle)

    if(perVehicle) toReturn
    else Array.fill(1)(toReturn.sum)
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check(!distanceIsSymmetric || ConstantRoutingDistance.isDistanceSymmetric(distanceMatrix),Some("distance matrix should be symmetric if invariant told so"))

    if(perVehicle){
      val values = computeValueFromScratch(routes.value)
      for (vehicle <- 0 to v-1){
        c.check(distance(vehicle).value == values(vehicle))
      }

      if(savedCheckpoint != null) {
        val values = computeValueFromScratch(savedCheckpoint)
        for (vehicle <- 0 to v - 1) {
          if(touchedRoutesSinceCheckpointArray(vehicle))
            c.check(savedValues(vehicle) == values(vehicle))
        }
      }

    }else{
      c.check(distance(0).value == computeValueFromScratch(routes.value)(0),Some("distance(0).value="+distance(0).value + " should== computeValueFromScratch(routes.value)(0)" + computeValueFromScratch(routes.value)(0)))
      if(savedCheckpoint != null){
        c.check(savedValues(0) == computeValueFromScratch(savedCheckpoint)(0))
      }
    }
  }
}
