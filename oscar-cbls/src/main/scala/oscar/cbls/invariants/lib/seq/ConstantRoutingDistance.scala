package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

/**
 * @param routes the routes of all the vehicles
 * @param v the number of vehicles in the model
 * @param distanceMatrix the matrix of distance, which is expected to be symmetric
 * @param distance it is either an array of one value, in which case it is the total distance run by all vehicle
 *                 or it can also be an array of size v. in this case it is the distance run by each vehicle, respectively.
 *                 the second option is computationally more expensive
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class ConstantRoutingDistance(routes:ChangingSeqValue,
                                       v:Int,
                                       distanceMatrix:Array[Array[Int]],
                                       distance:Array[CBLSIntVar], symmetricDistance:Boolean)
  extends Invariant() with SeqNotificationTarget{

  val perVehicle:Boolean = distance.size >1
  require(distance.length == 0 || distance.length == v)

  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  private val savedValues:Array[Int] = computeValueFromScratch(routes.value)
  private var savedCheckpoint = routes.value
  private val touchedRoutesSinceCheckpointArray:Array[Boolean] = Array.fill(v)(false)
  private var touchedRoutesSinceCheckpointList:QList[Int] = null

  affect(savedValues)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean){
    if(!digestUpdates(changes)) {
      affect(computeValueFromScratch(changes.newValue))
      savedCheckpoint = null
    }
    if(stableCheckpoint){
      saveCurrentCheckpoint(changes.newValue)
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val newSeq = changes.newValue

        val oldPrev = prev.newValue.valueAtPosition(pos-1).head

        val oldSuccIfNoLoop = prev.newValue.valueAtPosition(pos).head
        val oldSucc = if(oldSuccIfNoLoop < v) oldSuccIfNoLoop-1 else oldSuccIfNoLoop

        val oldDistance = distanceMatrix(oldPrev)(oldSucc)
        val newDistance = distanceMatrix(oldPrev)(value) + distanceMatrix(value)(oldSucc)

        if(perVehicle) {
          val vehicle = RoutingConventionMethods.searchVehicleReachingPosition(pos, newSeq, v)
          recordTouchedVehicle(vehicle)
          distance(vehicle) :+= (newDistance - oldDistance)
        }else{
          distance(0) :+= (newDistance - oldDistance)
        }
        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) false
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

          val deltaDistance = if(symmetricDistance) 0 else {
            //there is a flip and distance is asymmetric
            computeValueBetween(x.newValue, x.oldPosToNewPos(toIncluded),x.oldPosToNewPos(fromIncluded)) - computeValueBetween(prev.newValue, fromIncluded, toIncluded)
          }

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

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToIfNoLoop = prev.newValue.valueAtPosition(toIncluded + 1).head
          val oldSuccToValue = if (oldSuccToIfNoLoop < v) oldSuccToIfNoLoop-1 else oldSuccToIfNoLoop

          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldSuccAfterValue = RoutingConventionMethods.routingSuccPos2Val(after, prev.newValue, v)

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val oldHopAfterAfter = distanceMatrix(afterValue)(oldSuccAfterValue)

          val newHopBeforeMovedSegment = distanceMatrix(afterValue)(if(flip) toValue else fromValue)
          val newHopAfterMovedSegment = distanceMatrix(if(flip) toValue else fromValue)(oldSuccAfterValue)
          val newHopReplacingSegment = distanceMatrix(oldPrevFromValue)(oldSuccToValue)


          if(!perVehicle){
            val (deltaDistance) = if(symmetricDistance || !flip) 0 else {
              //there is a flip and distance is asymmetric
              computeValueBetween(x.newValue, x.oldPosToNewPos(toIncluded),x.oldPosToNewPos(fromIncluded)) - computeValueBetween(prev.newValue, fromIncluded, toIncluded)
            }

            distance(0) :+= (
              newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

          }else {
            val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded, prev.newValue, v)
            val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after, prev.newValue, v)
            assert(vehicleOfMovedSegment == RoutingConventionMethods.searchVehicleReachingPosition(toIncluded, prev.newValue,v))

            if (vehicleOfMovedSegment == targetVehicleOfMove) {
              //the segment is moved to the same vehicle

              val (deltaDistance) = if(symmetricDistance || !flip) 0 else {
                //there is a flip and distance is asymmetric
                computeValueBetween(x.newValue, x.oldPosToNewPos(toIncluded),x.oldPosToNewPos(fromIncluded)) - computeValueBetween(prev.newValue, fromIncluded, toIncluded)
              }

              recordTouchedVehicle(vehicleOfMovedSegment)
              distance(vehicleOfMovedSegment) :+= (
                newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                  - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

            } else {
              //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
              val oldCostInSegment = computeValueBetween(prev.newValue, fromIncluded, toIncluded)
              val newCostInSegment = if(symmetricDistance || !flip) oldCostInSegment else{
                //there is a flip and distance is asymmetric
                computeValueBetween(x.newValue, x.oldPosToNewPos(toIncluded),x.oldPosToNewPos(fromIncluded))
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

      case x@SeqUpdateRemoveValue(value : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false

        val positionOfDelete = x.position

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1).head //vehicles are never deleted

        val oldSuccValue = RoutingConventionMethods.routingSuccPos2Val(positionOfDelete-1, prev.newValue,v)

        val newDistance = distanceMatrix(oldPrevValue)(oldSuccValue)
        val oldDistanceBefore = distanceMatrix(oldPrevValue)(value)
        val oldDistanceAfter = distanceMatrix(value)(oldSuccValue)

        if(perVehicle){
          val vehicle = RoutingConventionMethods.searchVehicleReachingPosition(positionOfDelete, prev.newValue,v)
          recordTouchedVehicle(vehicle)
          distance(vehicle) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter))
        }else{
          distance(0) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter))
        }
        true

      case SeqUpdateSet(value : UniqueIntSequence) =>
        if(value quickEquals savedCheckpoint){
          restoreCheckpoint()
          true
        }else if (value quickEquals routes.value){
          true //we are starting from the previous value
        }else{
          false //impossible to go incremental
        }
    }
  }

  private def saveCurrentCheckpoint(s:UniqueIntSequence){
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

  private def forgetCheckpoint(){
    saveCurrentCheckpoint(null)
  }

  private def affect(value:Array[Int]){
    var currentV = 0
    while(currentV < v){
      distance(currentV) := value(currentV)
      currentV += 1
    }
  }

  //TODO: there is a O(1) way: labeled forward and backward nodes with their cumulated distance and use invalidation per vehicle in case more than one move is performed
  private def computeValueBetween(s:UniqueIntSequence, fromPosIncluded:Int, toPosIncluded:Int):Int = {
    var toReturn = 0
    var e = s.explorerAtPosition(fromPosIncluded).head

    while(e.position < toPosIncluded){
      val nextPos = e.next.head
      toReturn += distanceMatrix(e.value)(nextPos.value)
    }
    toReturn
  }

  private def computeValueFromScratch(s:UniqueIntSequence):Array[Int] = {
    val toReturn = Array.fill(v)(0)
    val it = s.iterator

    var prevNode:Int = it.next()
    var currentVehicle:Int = prevNode

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        //finishing the circle
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(currentVehicle)
        currentVehicle = node
      }else{
        //continuing on the same vehicle
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(node)
      }
      prevNode = node
    }
    toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(currentVehicle)

    if(perVehicle) toReturn
    else Array.fill(1)(toReturn.sum)
  }

  override def checkInternals(c : Checker) : Unit = {
    c.check(checkSymmetry,Some("distance matrix should be symmetric if invariant told so"))

    if(perVehicle){
      val values = computeValueFromScratch(routes.value)
      for (vehicle <- 0 to v-1){
        c.check(distance(vehicle).value == values(vehicle))
      }

      if(savedCheckpoint != null) {
        val values = computeValueFromScratch(savedCheckpoint)
        for (vehicle <- 0 to v - 1) {
          if(touchedRoutesSinceCheckpointArray(vehicle))
            c.check(distance(vehicle).value == values(vehicle))
        }
      }

    }else{
      c.check(distance(0).value == computeValueFromScratch(routes.value)(0))
      if(savedCheckpoint != null){
        c.check(distance(0).value == computeValueFromScratch(savedCheckpoint)(0))
      }
    }
  }

  def checkSymmetry:Boolean = {
    if(!symmetricDistance) return true
    val pointRange = distanceMatrix.indices
    for(i <- pointRange){
      for(j <- pointRange){
        if(distanceMatrix(i)(j) != distanceMatrix(j)(i)) return false
      }
    }
    true
   }
}
