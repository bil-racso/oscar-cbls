package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.{IntSequence, UniqueIntSequence}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.SortedSet

/**
 * @param routes the routes of all the vehicles
 * @param v the number of vehicles in the model
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class RouteNodes(routes:ChangingSeqValue,
                 v:Int,
                 nodesOfVehicle:Array[CBLSSetVar])  //there is actually one more vehicle, for unrouted nodes.
  extends Invariant() with SeqNotificationTarget{

  val n = routes.maxValue

  registerStaticAndDynamicDependency(routes)
  finishInitialization()

  private val savedValues:Array[SortedSet[Int]] = null
  private var savedCheckpoint:IntSequence = null
  private val movedNodesSinceCheckpointArray:Array[Boolean] = Array.fill(n)(false)
  private var movedNodesSinceCheckpointList:QList[Int] = null
  private val vehicleOfNodeAtCheckpointForMovedPoints:Array[Int] = Array.fill(n)(0)
  private val vehicleOfNodeAfterMoveForMovedPoints:Array[Int] = Array.fill(n)(0)

  affect(computeValueFromScratch(routes.value))

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate){
    if(!digestUpdates(changes)) {
      affect(computeValueFromScratch(changes.newValue))
    }
  }

  private def digestUpdates(changes:SeqUpdate):Boolean = {
    val newValue = changes.newValue

    changes match {
      case SeqUpdateInsert(value : Int, pos : Int, prev : SeqUpdate) =>
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val insertedVehicle = RoutingConventionMethods.searchVehicleReachingPosition(pos,newValue,v)
        nodesOfVehicle(insertedVehicle) :+= value
        nodesOfVehicle(v) :-= value
        recordMovedPoint(value, v, insertedVehicle)

        true
      case x@SeqUpdateMove(fromIncluded : Int, toIncluded : Int, after : Int, flip : Boolean, prev : SeqUpdate) =>
        //on which vehicle did we move?
        //also from --> to cannot include a vehicle start.
        if(!digestUpdates(prev)) false
        else if(x.isNop) true
        else if(x.isSimpleFlip){
          true
        }else {
          val oldValue = prev.newValue
          val vehicleOfMovedSegment = RoutingConventionMethods.searchVehicleReachingPosition(fromIncluded,oldValue,v)
          val targetVehicleOfMove = RoutingConventionMethods.searchVehicleReachingPosition(after,oldValue,v)
          if(vehicleOfMovedSegment != targetVehicleOfMove){
            //we moved all the points to another vehicle
            for(movedValue <- x.movedValues) {
              nodesOfVehicle(vehicleOfMovedSegment) :-= movedValue
              nodesOfVehicle(targetVehicleOfMove) :+= movedValue
              recordMovedPoint(movedValue, vehicleOfMovedSegment, targetVehicleOfMove)
            }
          }
          true
        }

      case x@SeqUpdateRemove(position: Int, prev : SeqUpdate) =>
        //on which vehicle did we remove?
        //on which vehicle did we insert?
        if(!digestUpdates(prev)) return false
        val oldValue = prev.newValue
        val impactedVehicle = RoutingConventionMethods.searchVehicleReachingPosition(position,oldValue,v)
        val removedValue = x.removedValue
        nodesOfVehicle(impactedVehicle) :-= removedValue
        nodesOfVehicle(v) :+= removedValue
        recordMovedPoint(removedValue, impactedVehicle, v)
        true
      case SeqUpdateSet(value : IntSequence) =>
        //TODO: foireux si checkpoit!!
        false //impossible to go incremental
      case SeqUpdateLastNotified(value:IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateRollBackToCheckpoint(checkpoint) =>
        require(checkpoint quickEquals savedCheckpoint)
        restoreCheckpoint()
        true
    }
  }

  private def saveCurrentCheckpoint(s:IntSequence){
    savedCheckpoint = s
    while (movedNodesSinceCheckpointList!= null) {
      movedNodesSinceCheckpointArray(movedNodesSinceCheckpointList.head) = false
      movedNodesSinceCheckpointList = movedNodesSinceCheckpointList.tail
    }
  }

  private def restoreCheckpoint(){
    while (movedNodesSinceCheckpointList!= null) {
      val node= movedNodesSinceCheckpointList.head
      movedNodesSinceCheckpointArray(movedNodesSinceCheckpointList.head) = false
      movedNodesSinceCheckpointList = movedNodesSinceCheckpointList.tail
      nodesOfVehicle(vehicleOfNodeAfterMoveForMovedPoints(node)) :-= node
      nodesOfVehicle(vehicleOfNodeAtCheckpointForMovedPoints(node)) :+= node
    }
  }

  private def recordMovedPoint(node:Int, oldVehicle:Int, newVehicle:Int){
    require(oldVehicle != newVehicle)
    if(savedCheckpoint!= null) {
      if (!movedNodesSinceCheckpointArray(node)) {
        movedNodesSinceCheckpointList = QList(node, movedNodesSinceCheckpointList)
        movedNodesSinceCheckpointArray(node) = true
        vehicleOfNodeAtCheckpointForMovedPoints(node) = oldVehicle
      }
      vehicleOfNodeAfterMoveForMovedPoints(node) = newVehicle
    }
  }

  private def affect(value:Array[SortedSet[Int]]){
    var currentV = 0
    while(currentV < v){
      nodesOfVehicle(currentV) := value(currentV)
      currentV += 1
    }
  }

  private def computeValueFromScratch(s:IntSequence):Array[SortedSet[Int]] = {
    val toReturn = Array.fill(v)(SortedSet.empty[Int])
    val it = s.iterator
    var currentVehicle:Int = it.next()
    require(currentVehicle == 0)
    toReturn(0) = toReturn(0) + (0)

    while(it.hasNext){
      val node = it.next()
      if(node < v){
        //reaching a new vehicle start
        currentVehicle = node
      }
      //continuing on the same vehicle
      toReturn(currentVehicle) = toReturn(currentVehicle) + node
    }
    toReturn
  }

  override def checkInternals(c : Checker) : Unit = {
    val values = computeValueFromScratch(routes.value)
    for (vehicle <- 0 to v-1){
      c.check(nodesOfVehicle(vehicle) equals values(vehicle))
    }

    if(savedCheckpoint != null) {
      val nodesOfVehicleFromScratch = computeValueFromScratch(savedCheckpoint)
      for (node <- 0 to n) {
        if(movedNodesSinceCheckpointArray(node))
          c.check(nodesOfVehicleFromScratch(vehicleOfNodeAtCheckpointForMovedPoints(node)).contains(node))
      }
    }
  }
}
