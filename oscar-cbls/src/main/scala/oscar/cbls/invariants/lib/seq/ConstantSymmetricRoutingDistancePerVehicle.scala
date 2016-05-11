package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.UniqueIntSequence
import oscar.cbls.invariants.core.computation._


case class ConstantSymmetricRoutingDistancePerVehicle(routes:ChangingSeqValue, v:Int, distanceMatrix:Array[Array[Int]],distancePerVehicle:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  //TODO: do something smarter than saving all these values
  val savedValues:Array[Int] = computeValueFromScratch(routes.value)
  var savedCheckpoint = routes.value
  val touchedRoutesSinceCheckpointArray:Array[Boolean] = Array.fill(v)(false)
  var touchedRoutesSinceCheckpointList:QList[Int] = null

  affect(savedValues)

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate,stableCheckpoint:Boolean){
    if(!digestUpdates(changes)) {
      affect(savedValues)
      savedCheckpoint = null
    }
    if(stableCheckpoint){
      saveCurrentCheckpoint(changes.newValue)
    }
  }

  def searchVehicleReachingPosition(position:Int, seq:UniqueIntSequence):Int = {
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

  def digestUpdates(changes:SeqUpdate):Boolean = {
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
        distancePerVehicle(vehicle) :+= (newDistance - oldDistance)
        true
      case x@SeqMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) return false
        if(x.isNop)
          true
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

          distancePerVehicle(vehicleOfMovedSegment) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
            - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment))

          true
        }else {
          val vehicleOfMovedSegment = searchVehicleReachingPosition(fromIncluded, prev.newValue)
          val targetVehicleOfMove = searchVehicleReachingPosition(after, prev.newValue)

          //summing the moved segment (this is slow, but this invar computes the cost per vehicle) //TODO: can we do better?
          val costInSegment = computeValueBetween(prev.newValue, fromIncluded, toIncluded)

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToIfNoLoop = prev.newValue.valueAtPosition(toIncluded + 1).head
          val oldSuccToValue = if (oldSuccToIfNoLoop == vehicleOfMovedSegment + 1 && oldSuccToIfNoLoop < v) vehicleOfMovedSegment else oldSuccToIfNoLoop


          val fromValue = x.fromValue
          val toValue = x.toValue
          val afterValue = x.afterValue

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val newHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(toValue)
          val newHopAfterMovedSegment = distanceMatrix(fromValue)(oldSuccToValue)

          distancePerVehicle(vehicleOfMovedSegment) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
            - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment))

        }
        false
      case SeqRemoveValue(value : Int, prev : SeqUpdate) =>
      //on which vehicle did we remove?

      case Set(value : UniqueIntSequence) =>
        if(value quickSame savedCheckpoint){
          restoreCheckpoint()
          true
        }else if (value quickSame routes.value){
          true
        }else{
          false
        }
    }
  }

  def saveCurrentCheckpoint(s:UniqueIntSequence){
    savedCheckpoint = s
    while(touchedRoutesSinceCheckpointList!=null){
      touchedRoutesSinceCheckpointArray(touchedRoutesSinceCheckpointList.head) = false
      touchedRoutesSinceCheckpointList = touchedRoutesSinceCheckpointList.tail
    }
  }

  def restoreCheckpoint(){
    while(touchedRoutesSinceCheckpointList!=null){
      val v = touchedRoutesSinceCheckpointList.head
      distancePerVehicle(v) := savedValues(v)
      touchedRoutesSinceCheckpointArray(v) = false
      touchedRoutesSinceCheckpointList = touchedRoutesSinceCheckpointList.tail
    }
  }

  def recordTouchedVehicle(v:Int){
    if(savedCheckpoint!= null && !touchedRoutesSinceCheckpointArray(v)){
      savedValues(v) = distancePerVehicle(v).value
      touchedRoutesSinceCheckpointArray(v) = true
      touchedRoutesSinceCheckpointList = QList(v,touchedRoutesSinceCheckpointList)
    }
  }

  def forgetCheckpoint(){
    saveCurrentCheckpoint(null)
  }

  def affect(value:Array[Int]){
    var currentV = 0
    while(currentV < v){
      distancePerVehicle(currentV) := value(currentV)
      currentV += 1
    }
  }

  def computeValueBetween(s:UniqueIntSequence, fromPosIncluded:Int, toPosIncluded:Int):Int = {
    var toReturn = 0
    var e = s.explorerAtPosition(fromPosIncluded).head

    while(e.position < toPosIncluded){
      val nextPos = e.next.head
      toReturn += distanceMatrix(e.value)(nextPos.value)
    }
    toReturn
  }


  def computeValueFromScratch(s:UniqueIntSequence):Array[Int] = {
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
        //continuingthe same vehicle
        toReturn(currentVehicle) = toReturn(currentVehicle) + distanceMatrix(prevNode)(node)
      }
      prevNode = node
    }
    toReturn
  }
}
