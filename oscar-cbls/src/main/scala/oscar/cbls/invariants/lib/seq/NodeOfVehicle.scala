package oscar.cbls.invariants.lib.seq

import oscar.cbls.invariants.core.algo.quick.QList
import oscar.cbls.invariants.core.algo.seq.functional.{IntSequence, UniqueIntSequence}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.core.propagation.Checker

import scala.collection.immutable.SortedSet

object NodeOfVehicle{
  def apply(routes:ChangingSeqValue,v:Int):Array[CBLSSetVar] = {
    val model = routes.model
    val emptySet = SortedSet.empty[Int]
    val domain = routes.domain

    val nodesOfVehicle = Array.tabulate(v+1)((vehicle:Int) =>
      CBLSSetVar(model,
        emptySet,
        domain,
        if(vehicle== v) "unrouted nodes" else "nodes_o_vehicle_" + vehicle))

    new NodeOfVehicle(routes, nodesOfVehicle)

    nodesOfVehicle
  }
}

/**
 * @param routes the routes of all the vehicles
 *
 * This invariant relies on the vehicle model assumption:
 * there are v vehicles
 * They are supposed to start from point of values 0 to v-1
 * These values must always be present in the sequence in increasing order
 * they cannot be included within a moved segment
 */
class NodeOfVehicle(routes:ChangingSeqValue,
                    nodesOfVehicleOrUnrouted:Array[CBLSSetVar])  //there is actually one more vehicle, for unrouted nodes.
  extends Invariant() with SeqNotificationTarget{

  val v = nodesOfVehicleOrUnrouted.length-1
  val n = routes.maxValue

  registerStaticAndDynamicDependency(routes)
  finishInitialization()
  nodesOfVehicleOrUnrouted.foreach(_.setDefiningInvariant(this))

  private val savedValues:Array[SortedSet[Int]] = null
  private var savedCheckpoint:IntSequence = null
  private val movedNodesSinceCheckpointArray:Array[Boolean] = Array.fill(n)(false)
  private var movedNodesSinceCheckpointList:QList[Int] = null
  private val vehicleOfNodeAtCheckpointForMovedPoints:Array[Int] = Array.fill(n)(0)
  private val vehicleOfNodeAfterMoveForMovedPoints:Array[Int] = Array.fill(n)(0)

  affect(computeValueFromScratch(routes.value))

  override def notifySeqChanges(v: ChangingSeqValue, d: Int, changes:SeqUpdate){
    if(!digestUpdates(changes)) {
      dropCheckpoint()
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
        nodesOfVehicleOrUnrouted(insertedVehicle) :+= value
        nodesOfVehicleOrUnrouted(v) :-= value
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
              nodesOfVehicleOrUnrouted(vehicleOfMovedSegment) :-= movedValue
              nodesOfVehicleOrUnrouted(targetVehicleOfMove) :+= movedValue
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
        nodesOfVehicleOrUnrouted(impactedVehicle) :-= removedValue
        nodesOfVehicleOrUnrouted(v) :+= removedValue
        recordMovedPoint(removedValue, impactedVehicle, v)
        true
      case SeqUpdateSet(value : IntSequence) =>
        false //impossible to go incremental
      case SeqUpdateLastNotified(value:IntSequence) =>
        true //we are starting from the previous value
      case SeqUpdateDefineCheckpoint(prev,activeCheckpoint) =>
        if(!digestUpdates(prev)) {
          affect(computeValueFromScratch(changes.newValue))
        }
        saveCurrentCheckpoint(prev.newValue)
        true
      case SeqUpdateRollBackToCheckpoint(checkpoint) =>
        if(checkpoint == null) false //it has been dropped following a Set
        else {
          require(checkpoint quickEquals savedCheckpoint)
          restoreCheckpoint()
          true
        }
    }
  }

  private def dropCheckpoint(){
    saveCurrentCheckpoint(null)
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
      nodesOfVehicleOrUnrouted(vehicleOfNodeAfterMoveForMovedPoints(node)) :-= node
      nodesOfVehicleOrUnrouted(vehicleOfNodeAtCheckpointForMovedPoints(node)) :+= node
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
    while(currentV <= v){
      nodesOfVehicleOrUnrouted(currentV) := value(currentV)
      currentV += 1
    }
  }

  private def computeValueFromScratch(s:IntSequence):Array[SortedSet[Int]] = {
    val toReturn = Array.fill(v+1)(SortedSet.empty[Int])
    toReturn(v) = toReturn(v) ++ (v to n-1)
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
      toReturn(v) = toReturn(v) - node
    }
    toReturn
  }

  override def checkInternals(c : Checker) : Unit = {
    val values = computeValueFromScratch(routes.value)
    for (vehicle <- 0 to v){
      c.check(nodesOfVehicleOrUnrouted(vehicle).value equals values(vehicle), Some("error on vehicle " + v + " output-correct:" + (nodesOfVehicleOrUnrouted(vehicle).value.diff(values(vehicle))) + " correct-output:" + (values(vehicle).diff(nodesOfVehicleOrUnrouted(vehicle).value))))
    }

    if(savedCheckpoint != null) {
      val nodesOfVehicleFromScratch = computeValueFromScratch(savedCheckpoint)
      for (node <- 0 to n-1) {
        if(movedNodesSinceCheckpointArray(node))
          c.check(nodesOfVehicleFromScratch(vehicleOfNodeAtCheckpointForMovedPoints(node)).contains(node))
      }
    }
  }
}
