package oscar.cbls.invariants.lib.routing
/*
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._

class VehicleCapacity(routes:ChangingSeqValue,
                      v:Int,
                      deltaAtNode:Array[Int], //the initial content is zero + delta at starting node.
                      maxCapacity:Int,
                      violation:Array[CBLSIntVar]) //violation is the integral of overshoot over leaves, counted on nodeLeave
  extends Invariant() with SeqNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  this.finishInitialization()
  violation.foreach(_.setDefiningInvariant(this))

  val n = routes.maxValue + 1
  val vehicles = 0 until v

  computeAndAffectViolationsFromScratch(routes.value)

  var checkpoint:IntSequence = null
  val contentOutAtCheckpoint:Array[Int] = Array.fill(n)(0)
  val nodeToLevelToNumberOfReachOutForwardAtCheckpoint:Array[RedBlackTreeMap[Int]] = Array.fill(n)(null)

  val backwardContentIntAtCheckpoint:Array[Int] = Array.fill(n)(0)
  val nodeToLevelToNumberOfReachIntBackwardAtCheckpoint:Array[RedBlackTreeMap[Int]] = Array.fill(n)(null)

  var changedVehiclesSinceCheckpoint:QList[Int] = vehicles.foldLeft[QList[Int]](null)((acc,v) => QList(v,acc))
  val isVehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(true)
  var violationsAtCheckpoint:Array[Int] = Array.fill(v)(0)

  protected var vehicleSearcher:((IntSequence,Int)=>Int) = if(v == 1) ((_,_) => 0) else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  def saveCheckpoint(newCheckpoint:IntSequence){
    checkpoint = newCheckpoint
    for(vehicle <- changedVehiclesSinceCheckpoint){
      isVehicleChangedSinceCheckpoint(vehicle) = false
      doPrecomputeAtCheckpoint(vehicle)
      violationsAtCheckpoint(vehicle) = violation(vehicle).newValue
    }
    changedVehiclesSinceCheckpoint = null
  }

  def restoreCheckpoint(){
    for(vehicle <- changedVehiclesSinceCheckpoint){
      isVehicleChangedSinceCheckpoint(vehicle) = false
      violation(vehicle) := violationsAtCheckpoint(vehicle)
    }
    changedVehiclesSinceCheckpoint = null
  }

  def recordTouchedVehicle(vehicle:Int){
    if(!isVehicleChangedSinceCheckpoint(vehicle)){
      isVehicleChangedSinceCheckpoint(vehicle) = true
      changedVehiclesSinceCheckpoint = QList(vehicle,changedVehiclesSinceCheckpoint)
    }
  }

  def addToReachCount(level:Int,addTo:RedBlackTreeMap[Int]):RedBlackTreeMap[Int] = {
    addTo.insert(level,addTo.getOrElse(level,0))
  }

  def integralOfAboveThreshold(numberOfOccurrences:RedBlackTreeMap[Int],threshold:Int):Int = {
    var integral = 0
    var width = 0
    var positionOfIntegrator = Int.MaxValue
    var nextPosition = numberOfOccurrences.biggestPosition

    while(nextPosition match{
      case None => false
      case Some(position) =>
        val newPivot = position.key
        if(newPivot < threshold) false
        else{
          integral += width * (positionOfIntegrator - newPivot)
          width += position.value
          positionOfIntegrator = newPivot
          nextPosition = position.prev
          true
        }
    }){}
    //finished, we just need to close with the width
    integral += (positionOfIntegrator - threshold) * width
    integral
  }

  def integralOfFreeSpaceBelowThreshold(numberOfOccurrences:RedBlackTreeMap[Int],threshold:Int,maxWidth:Int):Int = {
    var integral = 0
    var width = 0
    var positionOfIntegrator = Int.MaxValue
    var nextPosition = numberOfOccurrences.biggestPosition

    while(nextPosition match{
      case None => false
      case Some(position) =>
        val newPivot = position.key
        if(newPivot < threshold) {
          if (positionOfIntegrator > threshold) {
            integral += width * (threshold - newPivot)
          } else {
            integral += width * (positionOfIntegrator - newPivot)
          }
        }
        width -= position.value
        positionOfIntegrator = newPivot
        nextPosition = position.prev
        width > 0 //continue if not full width reached
    }){}
    integral
  }

  def integralOfDeltaAboveThreshold(integralFrom:RedBlackTreeMap[Int],
                                    integralTo:RedBlackTreeMap[Int],
                                    threshold:Int):Int = {
    integralOfAboveThreshold(integralTo,threshold) - integralOfAboveThreshold(integralFrom,threshold)
  }

  def doPrecomputeAtCheckpoint(vehicle:Int){
    val explorerAtVehicleStart = checkpoint.explorerAtAnyOccurrence(vehicle).head
    contentOutAtCheckpoint(vehicle) = deltaAtNode(vehicle)
    nodeToLevelToNumberOfReachOutForwardAtCheckpoint(vehicle) = RedBlackTreeMap(List((deltaAtNode(vehicle),1)))

    var explorerOpt = explorerAtVehicleStart.next
    var prevNode = vehicle

    while(explorerOpt match{
      case None => false //finished
      case Some(explorer) =>
        val node = explorer.value
        if(node >= v){
          //continuing the same vehicle

          contentOutAtCheckpoint(node) = contentOutAtCheckpoint(prevNode) + deltaAtNode(node)
          nodeToLevelToNumberOfReachOutForwardAtCheckpoint(node) = addToReachCount(
            contentOutAtCheckpoint(node),
            nodeToLevelToNumberOfReachOutForwardAtCheckpoint(prevNode))

          prevNode = node
          explorerOpt = explorer.next
          true
        }else{
          //at the next vehicle
          false
        }
    }){}
  }

  def computeAndAffectViolationsFromScratch(seq:IntSequence){
    for(vehicle <- vehicles){
      violation(vehicle) := computeViolationFromScratchNoPrecompute(routes.value,vehicle)
    }
  }

  def computeViolationFromScratchNoPrecompute(seq:IntSequence,vehicle:Int):Int = {

    var viol = 0
    var currentContent = deltaAtNode(vehicle)
    var explorerOpt = checkpoint.explorerAtAnyOccurrence(vehicle).head.next

    while(explorerOpt match{
      case None => false //finished
      case Some(explorer) =>
        val node = explorer.value
        if(node >= v){
          //continuing the same vehicle

          currentContent = currentContent + deltaAtNode(node)
          if(currentContent > maxCapacity){
            //overshoot, add to violation, 1 hop
            viol += (currentContent - maxCapacity)
          }
          explorerOpt = explorer.next
          true
        }else{
          //at the next vehicle
          false
        }
    }){}

    viol
  }

  def computeViolationFromScratchOnSegment(seq:IntSequence,incomingContentAtFromNodeIncluded:Int,fromNodeIncluded:Int,toNodeIncluded:Int):Int = {
    var currentContent = incomingContentAtFromNodeIncluded
    var currentViolation = 0
    var explorer = seq.explorerAtAnyOccurrence(fromNodeIncluded).head
    while({
      val node = explorer.value
      currentContent += this.deltaAtNode(node)
      if(currentContent > maxCapacity){
        currentViolation += (currentContent - maxCapacity)
      }
      if(node != toNodeIncluded){
        explorer = explorer.next.head
        true
      } else false
    }){}

    currentViolation
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    if(!digestUpdates(changes,false)) {
      for(v <- 0 until this.v) recordTouchedVehicle(v)
      computeAndAffectViolationsFromScratch(changes.newValue)
    }
  }

  private def digestUpdates(changes:SeqUpdate,skipNewCheckpoints:Boolean):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive:Boolean) =>
        if(!digestUpdates(prev,true)){
          computeAndAffectViolationsFromScratch(changes.newValue)
        }
        saveCheckpoint(changes.newValue)
        true
      case SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence) =>
        require (checkpoint quickEquals checkpoint)
        restoreCheckpoint()
        true
      //TODO: handle the doubleInsert here!
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev,skipNewCheckpoints)) return false
        val newSeq = changes.newValue
        //on which vehicle did we insert?

        val deltaAtInsertedNode = deltaAtNode(value)
        if(deltaAtInsertedNode == 0) return true //no impact, actually

        val vehicle = vehicleSearcher(newSeq, pos)
        recordTouchedVehicle(vehicle)

        if(!isVehicleChangedSinceCheckpoint(vehicle)){
          //we can evaluate incrementally

        }else{
          // need to use from scratch procedure :(

        }

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev,skipNewCheckpoints)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          //this is a simple flip
          //but we need to use another integral, actually.

          val oldPrevFromValue = prev.newValue.valueAtPosition(fromIncluded - 1).head
          val oldSuccToValue = RoutingConventionMethods.routingSuccPos2Val(toIncluded,prev.newValue,v)

          val fromValue = x.fromValue
          val toValue = x.toValue

          val oldHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(fromValue)
          val oldHopAfterMovedSegment = distanceMatrix(toValue)(oldSuccToValue)
          val newHopBeforeMovedSegment = distanceMatrix(oldPrevFromValue)(toValue)
          val newHopAfterMovedSegment = distanceMatrix(fromValue)(oldSuccToValue)

          //for simple flip, there is no node cost to consider
          if(perVehicle) {
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
            val deltaDistance = if(distanceIsSymmetric) 0 else {
              //there is a flip and distance is asymmetric
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue)
              - computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded, toValue)
            }
            recordTouchedVehicle(vehicleOfMovedSegment)
            distance(vehicleOfMovedSegment) :+= (newHopBeforeMovedSegment + newHopAfterMovedSegment
              - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment) + deltaDistance)
          }else{//not per vehicle
          val deltaDistance = if(distanceIsSymmetric) 0 else {
              val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
              //there is a flip and distance is asymmetric
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue)
              - computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded, toValue)
            }
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
            else { //there is a flip and distance is asymmetric
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
              computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                toIncluded, toValue,
                fromIncluded, fromValue)
              -computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded, toValue)

            }
            distance(0) :+= (
              newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

          }else {
            //per vehicle, there might be some node cost to consider
            val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
            val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
            assert(vehicleOfMovedSegment == vehicleSearcher(prev.newValue,toIncluded))

            if (vehicleOfMovedSegment == targetVehicleOfMove) {
              //the segment is moved to the same vehicle, so we do not consider node cost here

              val (deltaDistance) = if(distanceIsSymmetric || !flip) 0 else {
                //there is a flip and distance is asymmetric
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  toIncluded, toValue,
                  fromIncluded, fromValue)
                - computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  fromIncluded, fromValue,
                  toIncluded, toValue)
              }

              recordTouchedVehicle(vehicleOfMovedSegment)
              distance(vehicleOfMovedSegment) :+= (
                newHopReplacingSegment + newHopBeforeMovedSegment + newHopAfterMovedSegment
                  - (oldHopBeforeMovedSegment + oldHopAfterMovedSegment + oldHopAfterAfter) + deltaDistance)

            } else {
              //moving a segment to another vehicle, and per vehicle required.

              //summing the moved segment (this is slow, but it is requested to compute the cost per vehicle)
              val oldCostInSegment = computeValueBetween(prev.newValue,
                vehicleOfMovedSegment,
                fromIncluded, fromValue,
                toIncluded,toValue)
              val newCostInSegment = if(distanceIsSymmetric || !flip) oldCostInSegment else{
                //there is a flip and distance is asymmetric
                computeValueBetween(prev.newValue,
                  vehicleOfMovedSegment,
                  toIncluded,toValue,
                  fromIncluded, fromValue)
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
          val vehicle = vehicleSearcher(prev.newValue,positionOfDelete)
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

}
*/