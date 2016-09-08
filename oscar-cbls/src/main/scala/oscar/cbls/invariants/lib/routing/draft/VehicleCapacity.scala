package oscar.cbls.invariants.lib.routing.draft
/*
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.RedBlackTreeMap
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.routing.RoutingConventionMethods

class VehicleCapacity(routes:ChangingSeqValue,
                      v:Int,
                      deltaAtNode:Array[Int], //the initial content is zero + delta at starting node.
                      maxCapacity:Int,
                      violation:Array[CBLSIntVar],
                      contentAtEndOfVehicleRoute:Array[CBLSIntVar]) //violation per vehicle is the integral of overshoot over leaves, counted on nodeLeave
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

  var changedVehiclesSinceCheckpoint:QList[Int] = vehicles.foldLeft[QList[Int]](null)((acc,v) => QList(v,acc))
  val isVehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(true)
  var violationsAtCheckpoint:Array[Int] = Array.fill(v)(0)
  val contentAtEndOfVehicleRouteAtCheckpoint:Array[Int] = null

  //this one must always be up-to-date!

  protected var vehicleSearcher:((IntSequence,Int)=>Int) = if(v == 1) ((_,_) => 0) else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  def saveCheckpoint(newCheckpoint:IntSequence){
    checkpoint = newCheckpoint
    for(vehicle <- changedVehiclesSinceCheckpoint){
      isVehicleChangedSinceCheckpoint(vehicle) = false
      doPrecomputeAtCheckpoint(vehicle)
      violationsAtCheckpoint(vehicle) = violation(vehicle).newValue
      contentAtEndOfVehicleRouteAtCheckpoint(vehicle) = contentAtEndOfVehicleRoute(vehicle).newValue
    }
    changedVehiclesSinceCheckpoint = null
  }

  def restoreCheckpoint(){
    for(vehicle <- changedVehiclesSinceCheckpoint){
      isVehicleChangedSinceCheckpoint(vehicle) = false
      violation(vehicle) := violationsAtCheckpoint(vehicle)
      contentAtEndOfVehicleRoute(vehicle) := contentAtEndOfVehicleRouteAtCheckpoint(vehicle)
    }
    changedVehiclesSinceCheckpoint = null
  }

  def recordTouchedVehicleSinceCheckpoint(vehicle:Int){
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

  def integralOfBetweenThresholds(numberOfOccurrences:RedBlackTreeMap[Int],
                                  lowerThresholdIncluded:Int,
                                  higherThresholdIncluded:Int):Int = {

    var integral = 0
    var width = 0
    var positionOfIntegrator = Int.MaxValue
    var nextPosition = numberOfOccurrences.biggestPosition

    while(nextPosition match{
      case None =>
        if(positionOfIntegrator > lowerThresholdIncluded){
          //need to add something
          if(positionOfIntegrator > higherThresholdIncluded){
            assert(integral == 0)
            integral = width * (higherThresholdIncluded - lowerThresholdIncluded + 1)
          }else{
            integral += width * (positionOfIntegrator - lowerThresholdIncluded + 1)
          }
        }
        false
      case Some(position) =>
        val newPivot = position.key
        val addedWidth = position.value
        if(newPivot > higherThresholdIncluded){
          //integration has not started and will not start this time
          width += addedWidth
          positionOfIntegrator = newPivot
          nextPosition = position.prev
          true
        }else if (newPivot >= lowerThresholdIncluded){
          //we are integrating and not ending
          if(positionOfIntegrator > higherThresholdIncluded){
            //we start the integration now
            assert(integral == 0)
            integral = width * (higherThresholdIncluded - newPivot)
          }else{
            //integration had already started
            integral += width * (positionOfIntegrator - newPivot)
          }
          width += addedWidth
          positionOfIntegrator = newPivot
          nextPosition = position.prev
          true
        }else{
          //strictly below integration threshold
          //finish the integration

          if(positionOfIntegrator > higherThresholdIncluded){
            //we had not started integrating yet, so integrate one step and done
            integral = width * (higherThresholdIncluded - lowerThresholdIncluded + 1)
          }else{
            assert(positionOfIntegrator > lowerThresholdIncluded)
            //we were integrating, so integration must be closed
            integral += width * (positionOfIntegrator - lowerThresholdIncluded + 1)
          }

          false
        }
    }){}

    assert(integral == integralOfAboveThreshold(numberOfOccurrences,higherThresholdIncluded)
      - integralOfAboveThreshold(numberOfOccurrences,lowerThresholdIncluded))

    integral
  }

  def integralOfDeltaBetweenThresholds(integralFrom:RedBlackTreeMap[Int],
                                       integralTo:RedBlackTreeMap[Int],
                                       lowerThresholdIncluded:Int,higherThresholdIncluded:Int):Int = {
    val toReturn =
      (integralOfBetweenThresholds(integralTo, lowerThresholdIncluded, higherThresholdIncluded) -
        integralOfBetweenThresholds(integralFrom,lowerThresholdIncluded, higherThresholdIncluded))

    assert(toReturn == integralOfDeltaAboveThreshold(integralFrom,integralTo, higherThresholdIncluded) -
      integralOfDeltaAboveThreshold(integralFrom, integralTo, lowerThresholdIncluded))
    toReturn
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
      recordTouchedVehicleSinceCheckpoint(vehicle)
      val (viol,contentEnd) = computeViolationFromScratchNoPrecompute(routes.value,vehicle)
      violation(vehicle) := viol
      contentAtEndOfVehicleRoute(vehicle) := contentEnd
    }
  }

  def computeViolationFromScratchNoPrecompute(seq:IntSequence,vehicle:Int):(Int,Int) = {

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

    (viol,currentContent)
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

  def computeViolationFromScratchOnSegmentBackward(seq:IntSequence,outgoingContentAtToNodeIncluded:Int,fromNodeIncluded:Int,toNodeIncluded:Int):Int = {
    var currentContent = outgoingContentAtToNodeIncluded
    var currentViolation = 0
    var explorer = seq.explorerAtAnyOccurrence(toNodeIncluded).head
    while({
      val node = explorer.value
      if(currentContent > maxCapacity){
        currentViolation += (currentContent - maxCapacity)
      }
      currentContent -= this.deltaAtNode(node)
      if(node != toNodeIncluded){
        explorer = explorer.prev.head
        true
      } else false
    }){}

    currentViolation
  }

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes: SeqUpdate) {
    if(!digestUpdates(changes,false)) {
      for(v <- 0 until this.v) recordTouchedVehicleSinceCheckpoint(v)
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
      //TODO: handle the double Insert by supporting deltas on precomputations
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev,skipNewCheckpoints)) return false
        val newSeq = changes.newValue
        //on which vehicle did we insert?

        val vehicle = vehicleSearcher(newSeq, pos)
        val deltaAtInsertedNode = deltaAtNode(value)

        if(deltaAtInsertedNode == 0) {
          recordTouchedVehicleSinceCheckpoint(vehicle)
          return true //no impact, actually
        }

        if(!isVehicleChangedSinceCheckpoint(vehicle)){
          //we can evaluate incrementally
          recordTouchedVehicleSinceCheckpoint(vehicle)

          val nodeAfterInsertedPoint = RoutingConventionMethods.routingSuccPos2Val(pos,changes.newValue,v)
          val nodeBeforeInsertedPoint = RoutingConventionMethods.routingPredPos2Val(pos,changes.newValue,v)

          if(nodeAfterInsertedPoint < v){
            //inserted at end of vehicle route
            assert(vehicle == nodeAfterInsertedPoint)

            val contentArrivingAtInsertedNode = contentOutAtCheckpoint(nodeBeforeInsertedPoint)
            val contentLeavingInsertedNode = contentArrivingAtInsertedNode + deltaAtNode(value)
            if(contentLeavingInsertedNode > maxCapacity){
              violation(vehicle) :+= (maxCapacity - contentLeavingInsertedNode)
            }

          }else{
            //somewhere in the vehicle route

            val lastPointOfVehicleRoute = RoutingConventionMethods.routingPredVal2Val(vehicle,changes.newValue,v)
            val integralFrom = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(nodeAfterInsertedPoint)
            val integralTo = nodeToLevelToNumberOfReachOutForwardAtCheckpoint(lastPointOfVehicleRoute)

            //this is also the old content when getting at nodeAfterInsertionPoint
            val contentArrivingAtInsertedNode = contentOutAtCheckpoint(nodeBeforeInsertedPoint)
            val contentLeavingInsertedNode = contentArrivingAtInsertedNode + deltaAtNode(value)

            /*val deltaViolationOnSegmentRemain = if(violation(vehicle).newValue == 0){
              integralOfDeltaAboveThreshold(integralFrom,integralTo, maxCapacity - deltaAtNode(value))
            }else
              integralOfDeltaBetweenThresholds(integralFrom,integralTo,maxCapacity-deltaAtNode(value),maxCapacity)
*/

            val integralOnSegmentRemainBeforeInsert =
              if(violation(vehicle).newValue == 0) 0
              else integralOfDeltaAboveThreshold(integralFrom,integralTo, maxCapacity)

            val integralOnSegmentRemainAfterInsert =
              integralOfDeltaAboveThreshold(integralFrom,integralTo, maxCapacity - deltaAtNode(value))


            val violationAtInsertedNode =
              if(contentLeavingInsertedNode <= maxCapacity) 0
              else (maxCapacity - contentLeavingInsertedNode)

            val deltaOnViolation = integralOnSegmentRemainAfterInsert + violationAtInsertedNode - integralOnSegmentRemainBeforeInsert
            violation(vehicle) :+= deltaOnViolation
          }

        }else{
          // need to use from scratch procedure :(

          val nodeAfterInsertedPoint = RoutingConventionMethods.routingSuccPos2Val(pos,changes.newValue,v)
          val nodeBeforeInsertedPoint = RoutingConventionMethods.routingPredPos2Val(pos,changes.newValue,v)
          val lastPointOfVehicleRoute = RoutingConventionMethods.routingPredVal2Val(vehicle,changes.newValue,v)

          val violationOnSegmentRemainIncludingInsertedNodeBeforeInsert =   if(violation(vehicle).newValue == 0) 0
          else  computeViolationFromScratchOnSegmentBackward(prev.newValue,contentAtEndOfVehicleRoute(vehicle).newValue,nodeAfterInsertedPoint,lastPointOfVehicleRoute)

          val violationOnSegmentRemainIncludingInsertedNodeAfterInsert =
            computeViolationFromScratchOnSegmentBackward(prev.newValue,contentAtEndOfVehicleRoute(vehicle).newValue + deltaAtNode(value),value,lastPointOfVehicleRoute)

          val deltaOnViolation = violationOnSegmentRemainIncludingInsertedNodeAfterInsert - violationOnSegmentRemainIncludingInsertedNodeBeforeInsert
          violation(vehicle) :+= deltaOnViolation
        }

        contentAtEndOfVehicleRoute(vehicle) :+= deltaAtNode(value)
        true

      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev,skipNewCheckpoints)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          val vehicle = vehicleSearcher(prev.newValue, fromIncluded)

          if(isVehicleChangedSinceCheckpoint(vehicle)){
            //from scratch procedure


          }else{
            //use pre-computation



          }


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


          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = vehicleSearcher(prev.newValue, fromIncluded)
          val targetVehicleOfMove = vehicleSearcher(prev.newValue, after)
          assert(vehicleOfMovedSegment == vehicleSearcher(prev.newValue,toIncluded))

          if (vehicleOfMovedSegment == targetVehicleOfMove) {
            //the segment is moved to the same vehicle, so we do not consider node cost here


          } else {
            //moving a segment to another vehicle


          }
        }

        true
      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        val removedValue = x.removedValue
        //node cost to be considered
        if(!digestUpdates(prev,skipNewCheckpoints)) return false


        true

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        true //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

}
*/