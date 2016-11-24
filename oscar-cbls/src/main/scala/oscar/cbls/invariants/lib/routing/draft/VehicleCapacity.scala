package oscar.cbls.invariants.lib.routing.draft

/*
import oscar.cbls.algo.boolArray.MagicBoolArrayWithFastIteratorOnTrueOverApproximated
import oscar.cbls.algo.quick.QList
import oscar.cbls.algo.rb.{RedBlackTreeMapExplorer, RedBlackTreeMap}
import oscar.cbls.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker
import oscar.cbls.invariants.lib.routing.RoutingConventionMethods

class VehicleCapacity(routes:ChangingSeqValue,
                      v:Int,
                      deltaAtNode:Array[Int], //the initial content of a vehicle is the delta at its starting node.
                      maxCapacity:Int, //shared by all vehicles
                      violation:Array[CBLSIntVar], //violation of each vehicle; the integral of overshoot over nodes
                      contentAtEndOfVehicleRoute:Array[CBLSIntVar]) //violation per vehicle is the integral of overshoot over leaves, counted on nodeLeave
  extends Invariant() with SeqNotificationTarget{

  registerStaticAndDynamicDependency(routes)
  this.finishInitialization()
  violation.foreach(_.setDefiningInvariant(this))

  val n = routes.maxValue + 1
  val vehicles = 0 until v

  computeAndAffectViolationsFromScratch(routes.value)

  //single checkpoint
  var checkpoint:IntSequence = null
  val contentOutAtCheckpoint:Array[Int] = Array.fill(n)(0)
  val nodeToLevelToNumberOfReachOutForwardAtCheckpoint:Array[RedBlackTreeMap[Int]] = Array.fill(n)(null)

  val changedVehiclesSinceCheckpoint = new MagicBoolArrayWithFastIteratorOnTrueOverApproximated(v,false)

  var violationsAtCheckpoint:Array[Int] = Array.fill(v)(0)
  val contentAtEndOfVehicleRouteAtCheckpoint:Array[Int] = null

  //this one must always be up-to-date!

  protected var vehicleSearcher:((IntSequence,Int)=>Int) = if(v == 1) ((_,_) => 0) else
    RoutingConventionMethods.cachedVehicleReachingPosition(routes.value, v)

  def saveCheckpoint(newCheckpoint:IntSequence){
    checkpoint = newCheckpoint
    for(vehicle <- changedVehiclesSinceCheckpoint.indicesAtTrue){
      doPrecomputeAtCheckpoint(vehicle)
      violationsAtCheckpoint(vehicle) = violation(vehicle).newValue
      contentAtEndOfVehicleRouteAtCheckpoint(vehicle) = contentAtEndOfVehicleRoute(vehicle).newValue
    }
    changedVehiclesSinceCheckpoint.all = false
  }

  def restoreCheckpoint(){
    for(vehicle <- changedVehiclesSinceCheckpoint.indicesAtTrue){
      violation(vehicle) := violationsAtCheckpoint(vehicle)
      contentAtEndOfVehicleRoute(vehicle) := contentAtEndOfVehicleRouteAtCheckpoint(vehicle)
    }
    changedVehiclesSinceCheckpoint.all = false
  }

  def recordTouchedVehicleSinceCheckpoint(vehicle:Int){
    changedVehiclesSinceCheckpoint(vehicle) = true
  }

  def addToReachCount(level:Int,addTo:RedBlackTreeMap[Int]):RedBlackTreeMap[Int] = {
    addTo.insert(level,addTo.getOrElse(level,0))
  }


  /**
   * computes the integral on x of(toFunction(x) - fromFunction(x)) with x in [minValueIncluded, maxValueIncluded]
   * @param fromFunction
   * @param toFunction
   * @param minValueIncluded
   * @param maxValueIncluded
   * @return
   */
  def computeIntegralInBounds(fromFunction:RedBlackTreeMap[Int], toFunction:RedBlackTreeMap[Int], minValueIncluded:Int,maxValueIncluded:Int):Int = {

    //the integral goes from high to low values

    //position of integrator is not included yet in the integral
    def computeIntegralInBoundsEnd(positionOfIntegrator:Int, width:Int, acc:Int):Int = {
      if(positionOfIntegrator >= maxValueIncluded){
        acc + width * (minValueIncluded - maxValueIncluded + 1)
      }else if (positionOfIntegrator >= minValueIncluded){
        acc + width * (positionOfIntegrator - minValueIncluded + 1)
      }else{
        acc
      }
    }

    def computeIntegralInBoundsWithOptFromNoTo(nextPositionOnFromOpt:Option[RedBlackTreeMapExplorer[Int]],
                                               positionOfIntegrator:Int, width:Int, acc:Int):Int = {
      nextPositionOnFromOpt match{
        case None => computeIntegralInBoundsEnd(positionOfIntegrator, width, acc)
        case Some(nextPositionOnFrom) => computeIntegralInBoundsWithFromNoTo(nextPositionOnFrom,
          positionOfIntegrator, width, acc)
      }
    }

    def computeIntegralInBoundsWithFromNoTo(nextPositionOnFrom:RedBlackTreeMapExplorer[Int],
                                            positionOfIntegrator:Int, width:Int, acc:Int):Int = {
      val pivotValue = nextPositionOnFrom.key
      val substractedWidth = nextPositionOnFrom.value

      if(pivotValue > maxValueIncluded) {
        //this pivot is till above the integration box
        computeIntegralInBoundsWithOptFromNoTo(
          nextPositionOnFrom.prev,
          pivotValue,
          width - substractedWidth,
          acc)
      }else{
        //this pivot is below or equal to the maxValueIncluded
        val valueAboveIncluded = if(positionOfIntegrator > maxValueIncluded) maxValueIncluded else positionOfIntegrator
        if(pivotValue >= minValueIncluded) {
          //just a square added, and carry on
          computeIntegralInBoundsWithOptFromNoTo(
            nextPositionOnFrom.prev,
            pivotValue,
            width - substractedWidth,
            acc + (valueAboveIncluded - pivotValue) * width)
        }else{
          acc + (valueAboveIncluded - minValueIncluded + 1) * width
          //add a square and finish
        }
      }
    }

    def computeIntegralInBoundsNoFromWithOptTo(nextPositionOnToOpt:Option[RedBlackTreeMapExplorer[Int]],
      positionOfIntegrator:Int, width:Int, acc:Int):Int = {
      nextPositionOnToOpt match{
        case None => computeIntegralInBoundsEnd(positionOfIntegrator, width, acc)
        case Some(nextPositionOnTo) => computeIntegralInBoundsNoFromWithTo(nextPositionOnTo,
          positionOfIntegrator, width, acc)
      }
    }

    def computeIntegralInBoundsNoFromWithTo(nextPositionOnTo:RedBlackTreeMapExplorer[Int],
                                            positionOfIntegrator:Int, width:Int, acc:Int):Int = {
      val pivotValue = nextPositionOnTo.key
      val addedWidth = nextPositionOnTo.value

      if(pivotValue > maxValueIncluded) {
        //this pivot is till above the integration box
        computeIntegralInBoundsNoFromWithOptTo(
          nextPositionOnTo.prev,
          pivotValue,
          width + addedWidth,
          acc)
      }else{
        //this pivot is below or equal to the maxValueIncluded
        val valueAboveIncluded = if(positionOfIntegrator > maxValueIncluded) maxValueIncluded else positionOfIntegrator
        if(pivotValue >= minValueIncluded) {
          //just a square added, and carry on
          computeIntegralInBoundsNoFromWithOptTo(
            nextPositionOnTo.prev,
            pivotValue,
            width + addedWidth,
            acc + (valueAboveIncluded - pivotValue) * width)
        }else{
          acc + (valueAboveIncluded - minValueIncluded + 1) * width
          //add a square and finish
        }
      }
    }

    def computeIntegralInBoundsWithFromWithTo(nextPositionOnFrom:RedBlackTreeMapExplorer[Int],
                                              nextPositionOnTo:RedBlackTreeMapExplorer[Int],
                                              positionOfIntegrator:Int, width:Int, acc:Int):Int = {
    }


    def computeIntegralInBoundsOptFromOptTo(nextPositionOnToOpt:Option[RedBlackTreeMapExplorer[Int]],
                                            nextPositionOnFromOpt:Option[RedBlackTreeMapExplorer[Int]],
                                               positionOfIntegrator:Int, width:Int, acc:Int):Int = {
      (nextPositionOnToOpt, nextPositionOnFromOpt) match {
        case (None, None) =>
          computeIntegralInBoundsEnd(positionOfIntegrator, width, acc)
        case (Some(nextPositionOnFrom), None) =>
          computeIntegralInBoundsWithFromNoTo(nextPositionOnFrom, positionOfIntegrator, width, acc)
        case (None, Some(nextPositionOnTo)) =>
          computeIntegralInBoundsNoFromWithTo(nextPositionOnTo, positionOfIntegrator, width, acc)
        case (Some(nextPositionOnFrom), Some(nextPositionOnTo)) =>
          computeIntegralInBoundsWithFromWithTo(nextPositionOnFrom, nextPositionOnTo, positionOfIntegrator, width, acc)
      }
    }

    computeIntegralInBoundsOptFromOptTo(fromFunction.biggestPosition,toFunction.biggestPosition, Int.MaxValue, 0,0)
  }


  def integralOfAboveOrEqualToThreshold(numberOfOccurrences:RedBlackTreeMap[Int],threshold:Int):Int = {
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

  def integralOfFreeSpaceBelowOrEqualToThreshold(numberOfOccurrences:RedBlackTreeMap[Int],threshold:Int,maxWidth:Int):Int = {
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

  def integralOfDeltaAboveOrEqualToThreshold(integralFrom:RedBlackTreeMap[Int],
                                             integralTo:RedBlackTreeMap[Int],
                                             threshold:Int):Int = {
    integralOfAboveOrEqualToThreshold(integralTo,threshold) - integralOfAboveOrEqualToThreshold(integralFrom,threshold)
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

  /**
   *
   * @param seq
   * @param vehicle
   * @return (the violation of the vehicle; the content of the vehicle when returning to its home)
   */
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
    if(!digestUpdates(changes)) {
      for(v <- 0 until this.v) recordTouchedVehicleSinceCheckpoint(v)
      computeAndAffectViolationsFromScratch(changes.newValue)
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev:SeqUpdate,isActive:Boolean, checkpointLevel) =>
        if(checkpointLevel == 0) {
          if (!digestUpdates(prev)) {
            computeAndAffectViolationsFromScratch(changes.newValue)
          }
          saveCheckpoint(changes.newValue)
          true
        }else{
          digestUpdates(prev)
        }
      case r@SeqUpdateRollBackToCheckpoint(checkpoint:IntSequence,checkpointLevel) =>
        if(checkpointLevel == 0) {
          require(checkpoint quickEquals checkpoint)

          restoreCheckpoint()
          true
        }else{
          digestUpdates(r.howToRollBack)
        }
      //TODO: handle the double Insert by supporting deltas on precomputations
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        if(!digestUpdates(prev)) return false
        val newSeq = changes.newValue
        //on which vehicle did we insert?

        val vehicle = vehicleSearcher(newSeq, pos)
        val deltaAtInsertedNode = deltaAtNode(value)

        if(deltaAtInsertedNode == 0) {
          recordTouchedVehicleSinceCheckpoint(vehicle)
          return true //no impact, actually
        }

        if(!changedVehiclesSinceCheckpoint(vehicle)){
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
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          val vehicle = vehicleSearcher(prev.newValue, fromIncluded)

          if(changedVehiclesSinceCheckpoint(vehicle)){
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
        if(!digestUpdates(prev)) return false


        true

      case SeqUpdateLastNotified(value:IntSequence) =>
        require(value quickEquals routes.value)
        true //we are starting from the previous value
      case SeqUpdateAssign(value : IntSequence) =>
        false //impossible to go incremental
    }
  }


  override def checkInternals(c : Checker) : Unit = {
    for(v <- vehicles){
      val (viol,contentAtEnd) = computeViolationFromScratchNoPrecompute(routes.value,v)
      c.check(violation(v).value == viol, Some("Error on violation of vehicle " + v + " expected:" + viol + " actual output:" + violation(v).value))
      c.check(contentAtEndOfVehicleRoute(v).value == contentAtEnd, Some("Error on content at end of vehicle route v:" + v + " expected:" + contentAtEnd + " actual output:" + contentAtEndOfVehicleRoute(v).value))
    }
  }
}
*/