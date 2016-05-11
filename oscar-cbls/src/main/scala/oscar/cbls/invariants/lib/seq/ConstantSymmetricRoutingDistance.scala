package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

/**
 * @param routes the routes of all the vehicles
 * @param v the number of vehicles in the model
 * @param distanceMatrix the matrix of distance, which is expected to be symmetric
 * @param distance it is either an array of one value, in whihc case it is the total disance run by all vehicle
 *                 or it can also be an array of size v. in this case it is the distance run by each vehicle, respectively.
 *                 the second option is computationally more expensive
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class ConstantSymmetricRoutingDistance(routes:ChangingSeqValue,
                                       v:Int,
                                       distanceMatrix:Array[Array[Int]],
                                       distance:Array[CBLSIntVar])
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

  private def searchVehicleReachingPosition(position:Int, seq:UniqueIntSequence):Int = {
    var upperVehicle = v-1
    var upperVehiclePosition = seq.positionOfValue(upperVehicle).head

    if(position > upperVehiclePosition) return upperVehicle

    var lowerVehicle = 0
    var lowerVehiclePosition = 0

    assert(seq.positionOfValue(lowerVehicle).head == 0)
    require(lowerVehiclePosition <= upperVehiclePosition)

    while(lowerVehicle + 1 < upperVehicle){
      val midVehicle = (lowerVehicle + upperVehicle) /2
      val midVehiclePosition = seq.positionOfValue(midVehicle).head
      if(midVehiclePosition == position) return midVehicle
      if(midVehiclePosition <= position){
        lowerVehicle = midVehicle
        lowerVehiclePosition = midVehiclePosition
      }else{
        upperVehicle = midVehicle
        upperVehiclePosition = midVehiclePosition
      }
    }
    lowerVehicle
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val newSeq = changes.newValue
        val vehicle = searchVehicleReachingPosition(pos, newSeq)
        val oldPrev = prev.newValue.valueAtPosition(pos-1).head
        val oldSuccIfNoLoop = prev.newValue.valueAtPosition(pos).head
        val oldSucc = if(oldSuccIfNoLoop == vehicle+1 && oldSuccIfNoLoop < v) vehicle else oldSuccIfNoLoop
        val oldDistance = distanceMatrix(oldPrev)(oldSucc)
        val newDistance = distanceMatrix(oldPrev)(value) + distanceMatrix(value)(oldSucc)

        recordTouchedVehicle(vehicle)
        distance(if(perVehicle) vehicle else 0) :+= (newDistance - oldDistance)
        true
      case x@SeqMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          //this is a simple flip
          val vehicleOfMovedSegment = searchVehicleReachingPosition(fromIncluded, prev.newValue)

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToIfNoLoop = prev.newValue.valueAtPosition(toIncluded + 1).head
          val oldSuccToValue = if (oldSuccToIfNoLoop == vehicleOfMovedSegment + 1 && oldSuccToIfNoLoop < v) vehicleOfMovedSegment else oldSuccToIfNoLoop

          val fromValue = x.fromValue
          val toValue = x.toValue

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val newHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(toValue)
          val newHopAfterMovedSegment = distanceMatrix(fromValue)(oldSuccToValue)

          recordTouchedVehicle(vehicleOfMovedSegment)
          distance(if(perVehicle) vehicleOfMovedSegment else 0) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
            - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment))

          true
        }else {
          val vehicleOfMovedSegment = searchVehicleReachingPosition(fromIncluded, prev.newValue)
          val targetVehicleOfMove = searchVehicleReachingPosition(after, prev.newValue)
          assert(vehicleOfMovedSegment == searchVehicleReachingPosition(toIncluded, prev.newValue))

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToIfNoLoop = prev.newValue.valueAtPosition(toIncluded + 1).head
          val oldSuccToValue = if (oldSuccToIfNoLoop == vehicleOfMovedSegment + 1 && oldSuccToIfNoLoop < v) vehicleOfMovedSegment else oldSuccToIfNoLoop

          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldSuccAfterValue = prev.newValue.valueAtPosition(after + 1) match{
            case None => targetVehicleOfMove
            case Some(x) => if (x == vehicleOfMovedSegment + 1 && x < v) targetVehicleOfMove else x
          }

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val oldHopAfterAfter = distanceMatrix(afterValue)(oldSuccAfterValue)

          val newHopBeforeMovedSegment = distanceMatrix(afterValue)(if(flip) toValue else fromValue)
          val newHopAfterMovedSegment = distanceMatrix(if(flip) toValue else fromValue)(oldSuccAfterValue)
          val newHopReplacingSegment = distanceMatrix(oldPrevFromValue)(oldSuccToValue)

          if(!perVehicle || vehicleOfMovedSegment == targetVehicleOfMove){

            recordTouchedVehicle(vehicleOfMovedSegment)
            distance(if(perVehicle) vehicleOfMovedSegment else 0) :+= (
              newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter))

          }else {
            //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
            //TODO: check if a better (read: O(1)) solution can be found with this paradigm.
            val costInSegment = computeValueBetween(prev.newValue, fromIncluded, toIncluded)

            recordTouchedVehicle(vehicleOfMovedSegment)
            distance(vehicleOfMovedSegment) :+= (
              newHopReplacingSegment - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + costInSegment))

            recordTouchedVehicle(targetVehicleOfMove)
            distance(targetVehicleOfMove) :+= (
              newHopBeforeMovedSegment + costInSegment + newHopAfterMovedSegment - oldHopAfterAfter)
          }
          true
        }

      case x@SeqRemoveValue(value : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false

        val positionOfDelete = x.position

        val vehicle = searchVehicleReachingPosition(positionOfDelete, prev.newValue)

        val oldPrevValue = prev.newValue.valueAtPosition(positionOfDelete-1).head

        val oldSuccValue = prev.newValue.valueAtPosition(positionOfDelete) match{
          case None => vehicle
          case Some(x) => if(x == vehicle+1 && x < v) vehicle else x
        }

        val newDistance = distanceMatrix(oldPrevValue)(oldSuccValue)
        val oldDistanceBefore = distanceMatrix(oldPrevValue)(value)
        val oldDistanceAfter = distanceMatrix(value)(oldSuccValue)

        recordTouchedVehicle(vehicle)
        distance(if(perVehicle) vehicle else 0) :+= (newDistance - (oldDistanceBefore + oldDistanceAfter))
        true

      case Set(value : UniqueIntSequence) =>
        if(value quickSame savedCheckpoint){
          restoreCheckpoint()
          true
        }else if (value quickSame routes.value){
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
}
