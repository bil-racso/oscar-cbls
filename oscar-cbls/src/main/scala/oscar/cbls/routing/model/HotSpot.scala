package oscar.cbls.routing.model

import oscar.cbls.routing.neighborhood.VRPMove
import oscar.cbls.search.move.Move

import scala.collection.immutable.SortedSet

trait HotSpot extends VRP with RoutedAndUnrouted with PositionInRouteAndRouteNr{

  var lastHotSpotStep:HotSpotHistoryStep = new HotSpotHistoryStep()

  def updateHotSpotAnyMove(m:Move): Unit ={
    m match{
      case i:VRPMove with HotSpottingInfo =>
        updateHotSpotAfterMove(i)
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

  def newHotSpotView = new HotSpotView(
    hotVehicles = SortedSet.empty[Int] ++ this.vehicles,
    hotUnroutedPoints =  this.unrouted.value,
    this)
}

trait HotSpottingInfo{
  def stablePointsOfImpactedVehicles:List[Int]
  def unroutedPoints:List[Int]
}

case class HotSpotHistoryStep(updatedVehicles:List[Int]=Nil,
                              unroutedPoints:List[Int]=Nil,
                              var nextStep:HotSpotHistoryStep = null)

class HotSpotView(var hotVehicles:SortedSet[Int],
                  var hotUnroutedPoints:SortedSet[Int],
                  vrp:VRP with HotSpot){

  var lastDigestedUpdate = vrp.lastHotSpotStep

  def hotNodesByVehicleWithConsumption:Iterable[Int] = new HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp,this)

  def hotUnroutedWithConsumption:Iterable[Int] = new HotspotIterableOnUnroutedNodesWithConsumption(vrp,this)
  def updateFromHotSpot{
    while(lastDigestedUpdate.nextStep != null){
      lastDigestedUpdate = lastDigestedUpdate.nextStep
      hotVehicles ++= lastDigestedUpdate.updatedVehicles
      hotUnroutedPoints ++= lastDigestedUpdate.unroutedPoints
    }
  }
}

class HotspotIterableOnRoutedNodesByVehicleWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterable[Int]{
  override def iterator: Iterator[Int] = new RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotView)
}

class HotspotIterableOnUnroutedNodesWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterable[Int]{
  override def iterator: Iterator[Int] = new UnroutedIteratorWithConsumption(vrp:VRP,origin:HotSpotView)
}

class UnroutedIteratorWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterator[Int]{
  val primaryIterator = origin.hotUnroutedPoints.iterator
  var lastReturnedOne = Int.MinValue
  override def hasNext: Boolean = primaryIterator.hasNext

  override def next(): Int = {
    if(lastReturnedOne != Int.MinValue){
      origin.hotUnroutedPoints -= lastReturnedOne
    }
    lastReturnedOne = primaryIterator.next()
    lastReturnedOne
  }
}

class RoutedNodeIteratorWithConsumption(vrp:VRP,origin:HotSpotView) extends Iterator[Int]{
  var remainingVehiclesIncludingCurrentOne:List[Int] = origin.hotVehicles.toList
  var remainingPointsOfVehicle:List[Int] = remainingVehiclesIncludingCurrentOne match{
    case Nil => Nil
    case h::t => vrp.getRouteOfVehicle(h)
  }

  override def hasNext: Boolean = {
    remainingPointsOfVehicle.nonEmpty ||
      (remainingVehiclesIncludingCurrentOne.nonEmpty &&
        remainingVehiclesIncludingCurrentOne.tail.nonEmpty)
  }

  override def next(): Int = {
    remainingPointsOfVehicle match{
      case Nil =>
        remainingVehiclesIncludingCurrentOne match{
          case h::h2::t =>
            origin.hotVehicles -= h
            remainingVehiclesIncludingCurrentOne = h2::t
            val newRoute = vrp.getRouteOfVehicle(h2)
            remainingPointsOfVehicle = newRoute.tail
            newRoute.head
          case _ => throw new Error("")
        }
      case h::t =>
        remainingPointsOfVehicle = t
        h
    }
  }
}
