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

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.rb.RedBlackTreeMap
import oscar.cbls.invariants.core.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.SortedSet

object NodeVehicleRestrictions{
  def apply(routes:ChangingSeqValue,v:Int, nodeVehicleRestrictions:List[(Int,Int)]):Array[CBLSIntVar] = {
    val violationPerVehicle =  Array.tabulate(v)(vehicle => CBLSIntVar(routes.model,name="violation_of_nodeRoute restriction_vehicle:" + vehicle))

    new NodeVehicleRestrictions(routes, v, nodeVehicleRestrictions, violationPerVehicle)

    violationPerVehicle
  }
}

class NodeVehicleRestrictions(routes:ChangingSeqValue,
                              v:Int,
                              nodeVehicleRestrictions:List[(Int,Int)],
                              violationPerVehicle:Array[CBLSIntVar])
  extends Invariant() with SeqNotificationTarget {

  val n = routes.maxValue+1
  val vehicles = 0 until v

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  for(v <- violationPerVehicle) v.setDefiningInvariant(this)

  private val nodeToVehiclesRestriction : Array[(Array[Boolean],QList[Int])] = Array.fill(n)(null)

  for ((node, vehicle) <- nodeVehicleRestrictions) {
    nodeToVehiclesRestriction(node) match{
      case null =>
        val restrictionArray = Array.fill(v)(false)
        restrictionArray(vehicle) = true
        nodeToVehiclesRestriction(node) = (restrictionArray,QList(vehicle))
      case (restrictionArray,restrictionList) =>
        if(!restrictionArray(vehicle)){
          restrictionArray(vehicle) = true
          nodeToVehiclesRestriction(node) = (restrictionArray,QList(vehicle,restrictionList))
        }
    }
  }

  def isAllowed(node : Int, vehicle : Int) = {
    val nodeRestrictions =nodeToVehiclesRestriction(node)
    nodeRestrictions == null || !nodeRestrictions._1(vehicle)
  }

  def isForbidden(node : Int, vehicle : Int) = {
    val nodeRestrictions =nodeToVehiclesRestriction(node)
    nodeRestrictions != null && nodeRestrictions._1(vehicle)
  }

  def forbiddenVehicles(node:Int):Iterable[Int] = {
    val nodeRestrictions =nodeToVehiclesRestriction(node)
    if(nodeRestrictions == null) None
    else nodeRestrictions._2
  }

  var checkpoint : IntSequence = null
  var violationAtCheckpoint:Array[Int] = Array.fill(v)(-1)

  //node n => vehicle v => number of node from start of vehicle reaching n that cannot be reached by vehicle v
  val precomputationAtCheckpoint:Array[Array[Int]] = Array.tabulate(n)(_=>Array.fill(v)(0))
  val vehicleChangedSinceCheckpoint:Array[Boolean] = Array.fill(v)(true)
  var changedVehicleSinceCheckpoint:QList[Int] = vehicles.foldLeft[QList[Int]](null)((acc,v) => QList(v,acc))

  def setVehicleChangedSinceCheckpoint(vehicle:Int) {
    if(!vehicleChangedSinceCheckpoint(vehicle)){
      vehicleChangedSinceCheckpoint(vehicle) = true
      changedVehicleSinceCheckpoint = QList(vehicle,changedVehicleSinceCheckpoint)
    }
  }

  def setAllVehicleChangedSinceCheckpoint(){
    for (vehicle <- vehicles) {
      setVehicleChangedSinceCheckpoint(vehicle)
    }
  }

  def updateAllInvalidPrecomputationsToCheckpoint(){
    while(changedVehicleSinceCheckpoint != null){
      val vehicle = changedVehicleSinceCheckpoint.head
      changedVehicleSinceCheckpoint = changedVehicleSinceCheckpoint.tail
      doUpdatePrecomputationToCheckpoint(vehicle)
    }
  }

  def doUpdateAllPrecomputationsToCheckpoint(){
    for (vehicle <- vehicles) {
      doUpdatePrecomputationToCheckpoint(vehicle)
    }
    changedVehicleSinceCheckpoint = null
  }

  def doUpdatePrecomputationToCheckpoint(vehicle:Int){
    vehicleChangedSinceCheckpoint(vehicle) = false
    val explorerAtVehicleStart = checkpoint.explorerAtAnyOccurrence(vehicle).head
    var restrictionsForPrev = precomputationAtCheckpoint(explorerAtVehicleStart.value)
    var explorerAtCurrentNode = explorerAtVehicleStart.next
    while(explorerAtCurrentNode match{
      case None => false
      case Some(position) =>
        val node = position.value
        val restrictionsForCurrent = precomputationAtCheckpoint(node)
        for(vehicle <- vehicles){
          restrictionsForCurrent(v) = restrictionsForPrev(v)
        }

        for(forbiddenVehicle <-  forbiddenVehicles(node)) {
          restrictionsForCurrent(forbiddenVehicle) += 1
        }
        restrictionsForPrev = restrictionsForCurrent
        explorerAtCurrentNode = position.next
        true
    }){}
  }

  def violationOnSegmentFromPrecomputation(fromValue:Int,toValue:Int,vehicle:Int):Int = {
    require(!vehicleChangedSinceCheckpoint(vehicle))
    val nbRejectionFrom = precomputationAtCheckpoint(fromValue)(vehicle)
    val nbRejectionTo = precomputationAtCheckpoint(toValue)(vehicle)
    nbRejectionTo - nbRejectionFrom
  }

  def violationOnNodesFromScratch(nodes:Iterable[Int],vehicle:Int):Int = {
    var toReturn = 0
    val it = nodes.iterator
    while(it.hasNext){
      val node = it.next()
      if(isForbidden(node,vehicle)) toReturn += 1
    }
    toReturn
  }

  def violationOnSegment(seqWithSegment:IntSequence, fromValue:Int, toValue:Int, vehicle:Int):Int = {
    if(vehicleChangedSinceCheckpoint(vehicle)){
      violationOnNodesFromScratch(
        seqWithSegment.valuesBetweenPositions(
          seqWithSegment.positionOfAnyOccurrence(fromValue).head,
          seqWithSegment.positionOfAnyOccurrence(toValue).head),vehicle)
    }else{
      violationOnSegmentFromPrecomputation(fromValue,toValue,vehicle)
    }
  }

  def violationOnVehicle(vehicle:Int,seq:IntSequence):Int = {
    if(vehicleChangedSinceCheckpoint(vehicle)){
      violationOnNodesFromScratch(
        seq.valuesBetweenPositions(
          seq.positionOfAnyOccurrence(vehicle).head,
          if(vehicle == v-1) seq.size-1 else seq.positionOfAnyOccurrence(vehicle+1).head),
        vehicle)
    }else{
      violationOnSegmentFromPrecomputation(vehicle,if(vehicle == v-1) seq.last else vehicle+1,vehicle)
    }
  }

  override def notifySeqChanges(v : ChangingSeqValue, d : Int, changes : SeqUpdate) {
    if (!digestUpdates(changes, false)) {
      setAllVehicleChangedSinceCheckpoint()
      for (vehicle <- vehicles) {
        violationPerVehicle(vehicle) := violationOnVehicle(vehicle:Int,changes.newValue)
      }
    }
  }

  private def digestUpdates(changes : SeqUpdate, skipNewCheckpoints : Boolean) : Boolean = {
    changes match {
      case SeqUpdateDefineCheckpoint(prev : SeqUpdate, isActive : Boolean) =>
        if(skipNewCheckpoints) {
          digestUpdates(prev, true)
        }else{
          if (!digestUpdates(prev, true)) {
            this.checkpoint = prev.newValue
            doUpdateAllPrecomputationsToCheckpoint()
            for(vehicle <- vehicles) violationPerVehicle(vehicle) := violationOnVehicle(vehicle,checkpoint)
          }else {
            this.checkpoint = prev.newValue
            updateAllInvalidPrecomputationsToCheckpoint()
          }
          true
        }
      case SeqUpdateRollBackToCheckpoint(checkpoint : IntSequence) =>
        require(checkpoint quickEquals this.checkpoint)
        while(changedVehicleSinceCheckpoint!=null){
          val vehicle = changedVehicleSinceCheckpoint.head
          changedVehicleSinceCheckpoint = changedVehicleSinceCheckpoint.tail
          violationPerVehicle(vehicle) := violationAtCheckpoint(vehicle)
          vehicleChangedSinceCheckpoint(vehicle) = false
        }

        true
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if (!digestUpdates(prev, skipNewCheckpoints)) return false

        val vehicleOfInsert = RoutingConventionMethods.searchVehicleReachingPosition(pos, prev.newValue, v)
        if (isForbidden(value, vehicleOfInsert)) {
          violationPerVehicle(vehicleOfInsert) :+= 1
        }
        setVehicleChangedSinceCheckpoint(vehicleOfInsert)

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if (!digestUpdates(prev, skipNewCheckpoints)) false
        else if (x.isNop) true
        else if (x.isSimpleFlip) {
          //this is a simple flip, no change on vehicle violation, since obeys routing rule!
          val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded, prev.newValue, v)
          setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
          true
        } else {
          //per vehicle, there might be some node cost to consider
          val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded, prev.newValue, v)
          val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after, prev.newValue, v)
          assert(vehicleOfMovedSegment == RoutingConventionMethods.searchVehicleReachingPosition(toIncluded, prev.newValue, v))

          if (vehicleOfMovedSegment == targetVehicleOfMove) {
            //moving within the vehicle; same as flip, actually
            setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
          }else {
            //moving accross vehicles

            val fromValue = x.fromValue
            val toValue = x.toValue

            if(violationPerVehicle(vehicleOfMovedSegment).newValue != 0){
              //check if we decrease the violation on that vehicle
              violationPerVehicle(vehicleOfMovedSegment) :-= violationOnSegment(prev.newValue, fromValue, toValue, vehicleOfMovedSegment)
            }//else we do not introduce a violation on that vehicle by removing some nodes, right?

            violationPerVehicle(targetVehicleOfMove) :-= violationOnSegment(prev.newValue, fromValue, toValue, targetVehicleOfMove)

            setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
            setVehicleChangedSinceCheckpoint(targetVehicleOfMove)
          }
          true
        }

      case x@SeqUpdateRemove(position : Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        if (!digestUpdates(prev, skipNewCheckpoints)) return false
        val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(position, prev.newValue, v)
        if(isForbidden(x.removedValue,vehicleOfMovedSegment)){
          violationPerVehicle(vehicleOfMovedSegment) :-= 1
        }
        setVehicleChangedSinceCheckpoint(vehicleOfMovedSegment)
        true

      case SeqUpdateLastNotified(value : IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateSet(value : IntSequence) =>
        false //impossible to go incremental
    }
  }

  override def checkInternals(c : Checker) : Unit = {

    val seq = routes.value
    for(vehicle <- vehicles){
      c.check(violationPerVehicle(vehicle).value == violationOnVehicle(vehicle,seq))

      val oldUpToDate = vehicleChangedSinceCheckpoint(vehicle)
      vehicleChangedSinceCheckpoint(vehicle) = true
      c.check(violationPerVehicle(vehicle).value == violationOnVehicle(vehicle,seq))
      vehicleChangedSinceCheckpoint(vehicle) = oldUpToDate
    }

    if(checkpoint != null){
      for(vehicle <- vehicles){
        val oldUpToDate = vehicleChangedSinceCheckpoint(vehicle)
        vehicleChangedSinceCheckpoint(vehicle) = true
        c.check(violationAtCheckpoint(vehicle) == violationOnVehicle(vehicle,checkpoint))
        vehicleChangedSinceCheckpoint(vehicle) = oldUpToDate
      }
    }
  }
}

