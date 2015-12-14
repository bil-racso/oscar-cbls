package oscar.cbls.routing.model

import oscar.cbls.routing.neighborhood.VRPMove
import oscar.cbls.search.move.{CompositeMove, Move}

import scala.collection.immutable.SortedSet
import scala.collection.mutable.Queue

trait HotSpot extends VRP with RoutedAndUnrouted with PositionInRouteAndRouteNr{

  var lastHotSpotStep:HotSpotHistoryStep = new HotSpotHistoryStep()

  def updateHotSpotAnyMove(m:Move): Unit ={
    m match{
      case i:VRPMove with HotSpottingInfo =>
        updateHotSpotAfterMove(i)
      case CompositeMove(moves, _, _) =>
        moves.foreach(updateHotSpotAnyMove)
      case _ => resetHotSpotState
    }
  }

  def updateHotSpotAfterMove(m: VRPMove with HotSpottingInfo){
    val newStep = new HotSpotHistoryStep(m.stablePointsOfImpactedVehicles.map(node => routeNr(node).value),m.unroutedPoints)
    lastHotSpotStep.nextStep = newStep
    lastHotSpotStep = newStep
  }

  def resetHotSpotState() {
    val newStep = new HotSpotHistoryStep(this.vehicles.toList,this.unrouted.value.toList)
    lastHotSpotStep.nextStep = newStep
    lastHotSpotStep = newStep
  }

  def newHotSpotView = new HotSpotView(this)
}

trait HotSpottingInfo{
  def stablePointsOfImpactedVehicles:List[Int]
  def unroutedPoints:List[Int]
}

case class HotSpotHistoryStep(updatedVehicles:List[Int]=Nil,
                              unroutedPoints:List[Int]=Nil,
                              var nextStep:HotSpotHistoryStep = null)

class HotSpotView(vrp:VRP with HotSpot){

  var hotVehiclesSet:SortedSet[Int] = SortedSet.empty[Int] ++ vrp.vehicles
  val hotVehicleQueue:Queue[Int] = new Queue[Int]
  var hotUnroutedPointsSet:SortedSet[Int] = vrp.unrouted.value
  val hotUnroutedPointsQueue:Queue[Int] = new Queue[Int]

  var lastDigestedUpdate = vrp.lastHotSpotStep


  def setFirstVehiceCold(): Unit ={
    hotVehiclesSet -= hotVehicleQueue.dequeue()
  }

  def setUnroutedNodeCold(node:Int): Unit ={
    require(node == hotUnroutedPointsQueue.head)
    hotUnroutedPointsQueue.dequeue()
    hotUnroutedPointsSet -= node
  }
  def setVehicleHot(v:Int): Unit ={
    if(!hotVehiclesSet.contains(v)){
      hotVehicleQueue.enqueue(v)
      hotVehiclesSet += v
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
    new HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp,this)

  def updateFromHotSpot{
    while(lastDigestedUpdate.nextStep != null){
      lastDigestedUpdate = lastDigestedUpdate.nextStep
      lastDigestedUpdate.updatedVehicles.foreach(setVehicleHot)
      lastDigestedUpdate.unroutedPoints.foreach(setUnroutedNodeHot)
    }
  }

  def rollHotVehicleForHotRestart(): Unit ={
    hotVehicleQueue.enqueue(hotVehicleQueue.dequeue())
  }
  def rollHotUnroutedForHotRestart(): Unit ={
    hotUnroutedPointsQueue.enqueue(hotUnroutedPointsQueue.dequeue())
  }
}

class HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterable[Int]{
  override def iterator: Iterator[Int] = {
    origin.rollHotUnroutedForHotRestart()
    new RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotView)
  }
}

class HotspotIterableOnUnroutedNodesWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterable[Int]{
  override def iterator: Iterator[Int] = {
    origin.rollHotVehicleForHotRestart()
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

class RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterator[Int]{
  //HotRestart is implemented by rolling the vehicle at each update of the HotSpot

  var remainingPointsOfVehicle:List[Int] =
    if(origin.hotVehicleQueue.isEmpty) Nil
    else vrp.getRouteOfVehicle(origin.hotVehicleQueue.head)

  override def hasNext: Boolean =
    remainingPointsOfVehicle.nonEmpty || origin.hotVehicleQueue.nonEmpty

  override def next(): Int = {
    remainingPointsOfVehicle match{
      case h::Nil =>
        origin.setFirstVehiceCold()
        remainingPointsOfVehicle = Nil
        h
      case h::t =>
        remainingPointsOfVehicle = t
        h
      case Nil =>
        remainingPointsOfVehicle =
          (if(origin.hotVehicleQueue.isEmpty) throw new Error("no next available, actually")
          else vrp.getRouteOfVehicle(origin.hotVehicleQueue.head))
        next()
    }
  }
}
