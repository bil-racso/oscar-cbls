package oscar.cbls.routing.model

import oscar.cbls.routing.neighborhood.VRPMove
import oscar.cbls.search.algo.HotRestart
import oscar.cbls.search.move.{CompositeMove, Move}

import scala.collection.immutable.SortedSet
import scala.collection.mutable.Queue

trait HotSpot extends VRP with RoutedAndUnrouted with PositionInRouteAndRouteNr{

  var lastHotSpotStep:HotSpotHistoryStep = new HotSpotHistoryStep()

  def updateHotSpotAnyMove(m:Move): Unit ={
    //println("XXXXXXXXXXXXXXXXXXX update hotSpot")
    m match{
      case i:VRPMove with HotSpottingInfo =>
        updateHotSpotAfterMove(i)
      case CompositeMove(moves, _, _) =>
        moves.foreach(updateHotSpotAnyMove)
      case _ => resetHotSpotState
    }
  }

  def updateHotSpotAfterMove(m: VRPMove with HotSpottingInfo){
    val impacted = m.impactedPoints
    val (unrouted,impactedVehicles) = impacted.foldLeft((List.empty[Int],List.empty[Int]))((acc,node) => {
      val routeOfPoint = routeNr(node).value
      if(routeOfPoint == V) ((node :: acc._1),acc._2)         //unrouted
      else (acc._1,routeOfPoint :: acc._2)        //routed, vehicle V
    })

    val newStep = new HotSpotHistoryStep(impactedVehicles,unrouted)
    lastHotSpotStep.nextStep = newStep
    lastHotSpotStep = newStep
  }

  def resetHotSpotState() {
    val newStep = new HotSpotHistoryStep(this.vehicles.toList,this.unrouted.value.toList)
    lastHotSpotStep.nextStep = newStep
    lastHotSpotStep = newStep
  }

  def newHotSpotView(hotRestart:Boolean) = new HotSpotView(this,hotRestart)
}

trait HotSpottingInfo{
  def impactedPoints:List[Int]
}

case class HotSpotHistoryStep(updatedVehicles:List[Int]=Nil,
                              unroutedPoints:List[Int]=Nil,
                              var nextStep:HotSpotHistoryStep = null)

class HotSpotView(vrp:VRP with HotSpot, hotRestart:Boolean){

  var lastIteratedPoint:Int = -1

  var hotVehiclesSet:SortedSet[Int] = SortedSet.empty[Int] ++ vrp.vehicles
  val hotVehicleArray:Array[Boolean] = Array.fill(vrp.V)(true)
  val hotVehicleQueue:Queue[Int] = new Queue[Int] ++ vrp.vehicles
  var hotUnroutedPointsSet:SortedSet[Int] = vrp.unrouted.value
  val hotUnroutedPointsQueue:Queue[Int] = new Queue[Int]

  var lastDigestedUpdate = vrp.lastHotSpotStep

  def setFirstVehiceCold(): Unit ={
    val vehicle = hotVehicleQueue.dequeue()
    hotVehiclesSet -= vehicle
    hotVehicleArray(vehicle) = false
    //    println("after setting first vehicle cold:" + hotVehicleQueue)
  }

  def setUnroutedNodeCold(node:Int): Unit ={
    require(node == hotUnroutedPointsQueue.head)
    hotUnroutedPointsQueue.dequeue()
    hotUnroutedPointsSet -= node
  }
  def setVehicleHot(v:Int): Unit ={
    if(!hotVehicleArray(v)){
      hotVehicleQueue.enqueue(v)
      hotVehiclesSet += v
      hotVehicleArray(v) = true
    }
  }
  def setUnroutedNodeHot(node:Int): Unit ={
    if(! hotUnroutedPointsSet.contains(node)){
      hotUnroutedPointsQueue.enqueue(node)
      hotUnroutedPointsSet += node
    }
  }

  def hotUnroutedWithConsumption:Iterable[Int] =
    new HotspotIterableOnUnroutedNodesWithConsumption(vrp, this)

  def hotNodesByVehicleWithConsumption:Iterable[Int] =
    new HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp, this, lastIteratedPoint)

  def updateFromHotSpot{
    while(lastDigestedUpdate.nextStep != null){
      lastDigestedUpdate = lastDigestedUpdate.nextStep
      lastDigestedUpdate.updatedVehicles.foreach(setVehicleHot)
      lastDigestedUpdate.unroutedPoints.foreach(setUnroutedNodeHot)
    }
    if(hotRestart){
      if(hotVehicleQueue.nonEmpty) hotVehicleQueue.enqueue(hotVehicleQueue.dequeue())
      if (hotUnroutedPointsQueue.nonEmpty) hotUnroutedPointsQueue.enqueue(hotUnroutedPointsQueue.dequeue())
    }
  }
}

class HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp:VRP,origin:HotSpotView,lastIteratedPoint:Int) extends Iterable[Int]{

  override def iterator: Iterator[Int] = {
    origin.updateFromHotSpot
    new RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotView,lastIteratedPoint)
  }
}

class HotspotIterableOnUnroutedNodesWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterable[Int]{
  override def iterator: Iterator[Int] = {
    origin.updateFromHotSpot
    new UnroutedIteratorWithConsumption(vrp: VRP, origin: HotSpotView)
  }
}

class UnroutedIteratorWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterator[Int]{

  val primaryIterator = origin.hotUnroutedPointsQueue.iterator
  override def hasNext: Boolean = primaryIterator.hasNext

  override def next(): Int = {
    val toReturn = primaryIterator.next()
    origin.setUnroutedNodeCold(toReturn)
    toReturn
  }
}

class RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotView,lastIteratedPoint:Int) extends Iterator[Int]{
  //HotRestart is implemented by rolling the vehicle at each update of the HotSpot
  //TODO: we should improve on this by staying on the last vehicle, and performing a hot restart on the position in this vehicle. in order to set it cold as quickly as possible.

  /*  var remainingPointsOfVehicle:List[Int] =
      if(origin.hotVehicleQueue.isEmpty) Nil
      else vrp.getRouteOfVehicle(origin.hotVehicleQueue.head)*/


  var pointsOfVehicle:Iterator[Int] =
    HotRestart.hotRestartPreserveSequence((if(origin.hotVehicleQueue.isEmpty) Nil
    else vrp.getRouteOfVehicle(origin.hotVehicleQueue.head)),lastIteratedPoint).iterator

  //  println("YYYYYYYYYYYYYYYYYYYYYYYY RoutedNodeIteratorWithConsumption ")
  //  println("origin.hotVehicleQueue" + origin.hotVehicleQueue)

  override def hasNext: Boolean =
    pointsOfVehicle.hasNext || origin.hotVehicleQueue.nonEmpty

  override def next(): Int = {

    if(!pointsOfVehicle.hasNext) {
      pointsOfVehicle =  (if (origin.hotVehicleQueue.isEmpty) throw new Error("no next available, actually")
      else HotRestart.hotRestartPreserveSequence(vrp.getRouteOfVehicle(origin.hotVehicleQueue.head),lastIteratedPoint).iterator)

      return this.next()
    }

    val next = pointsOfVehicle.next()

    if(!pointsOfVehicle.hasNext) {
      origin.setFirstVehiceCold()
    }
    origin.lastIteratedPoint = next
    next
  }
}
