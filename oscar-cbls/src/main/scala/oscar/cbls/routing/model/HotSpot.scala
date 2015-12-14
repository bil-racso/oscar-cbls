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
    new HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp, this)

  def updateFromHotSpot{
    while(lastDigestedUpdate.nextStep != null){
      lastDigestedUpdate = lastDigestedUpdate.nextStep
      lastDigestedUpdate.updatedVehicles.foreach(setVehicleHot)
      lastDigestedUpdate.unroutedPoints.foreach(setUnroutedNodeHot)
    }
    if(hotRestart){
      hotVehicleQueue.enqueue(hotVehicleQueue.dequeue())
      hotUnroutedPointsQueue.enqueue(hotUnroutedPointsQueue.dequeue())
    }
  }
}

class HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterable[Int]{
  override def iterator: Iterator[Int] = {
    new RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotView)
  }
}

class HotspotIterableOnUnroutedNodesWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterable[Int]{
  override def iterator: Iterator[Int] = {
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
