package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model.{HotSpottingInfo, NodesOfVehicle, PositionInRouteAndRouteNr, VRP}
import oscar.cbls.search.algo.{Pairs, HotRestart}
import scala.collection.immutable.{SortedMap, SortedSet}

/**
 * swaps segments of different vehicles
 * THIS IS EXPERIMENTAL
 */
class SegmentExchange(vrp: VRP with PositionInRouteAndRouteNr with NodesOfVehicle,
                      relevantNeighbors:()=>Int=>Iterable[Int],
                      vehicles:() => List[Int],
                      neighborhoodName:String = "SegmentExchange",
                      hotRestart:Boolean,
                      best:Boolean = false) extends EasyRoutingNeighborhood[SegmentExchangeMove](best,vrp,neighborhoodName) {

  var beforeFirstSegment: Int = 0
  var endFirstSegment: Int = 0
  var reverseFirstSegment:Boolean = false
  var beforeSecondSegment: Int = 0
  var endSecondSegment: Int = 0
  var reverseSecondSegment:Boolean = false
  var startVehicle = 0

  override def exploreNeighborhood() {
    val iterationSchemeOnVehicles = if (hotRestart && !best) HotRestart(vehicles(), startVehicle) else vehicles()

    cleanRecordedMoves()

    val relevantNeighborsNow = relevantNeighbors()

    for ((vehicle1, otherVehicles) <- Pairs.makeAllHeadAndTails(iterationSchemeOnVehicles.toList)) {
      val otherVehiclesSet:SortedSet[Int] = SortedSet.empty[Int] ++ otherVehicles
      val routeOfVehicle1 = vrp.getRouteOfVehicle(vehicle1)
      //cluster the closest by the otherVehicles
      val routeWithRelevantNeighborsAndTheirPositionInTheirRouteGroupedByVehicles = routeOfVehicle1.map(node =>
        (node, relevantNeighborsNow(node)
          .filter(otherVehiclesSet.contains(_))
          .map(node => (node,vrp.positionInRoute(node).value))
          .groupBy(nodeAndPosition => vrp.routeNr(nodeAndPosition._1).value)))

      //iterating over the route to search the first segment beforeMovePoint
      for (((beforeFirstSegment, relevantNeighbors1GroupedByRoute), tail) <-
           Pairs.makeAllHeadAndTails(routeWithRelevantNeighborsAndTheirPositionInTheirRouteGroupedByVehicles)) {
        this.beforeFirstSegment = beforeFirstSegment

        for ((endFirstSegment, relevantNeighbours2GroupedByRoute) <- tail) {
          this.endFirstSegment = endFirstSegment
          val relevantVehicles = relevantNeighbors1GroupedByRoute.keySet.intersect(relevantNeighbours2GroupedByRoute.keySet)
          for (otherVehicle <- relevantVehicles) {
            val relevantNeighborsForSeg1BeforeStart: List[(Int, Int)] = relevantNeighbors1GroupedByRoute(otherVehicle).toList
            val relevantNeighborsForSeg1End: List[(Int, Int)] = relevantNeighbours2GroupedByRoute(otherVehicle).toList
            //TODO: on devrait prendre .next en fait.
            val closestInPairs = Pairs.zipIntoAllPossiblePairs(relevantNeighborsForSeg1BeforeStart, relevantNeighborsForSeg1End).filter(
            { case ((node1: Int, position1: Int), (node2: Int, position2: Int)) => node1 != node2 })

            //node1,node2,true if swapped, false otherwise
            val closestInPairsInCorrectOrderSwapInfo = closestInPairs.map(
            { case ((node1: Int, position1: Int), (node2: Int, position2: Int)) =>
              if (position1 < position2) (node1, node2, false)
              else (node2, node1, true)
            })

            for ((node1, node2, swapped) <- closestInPairsInCorrectOrderSwapInfo) {

              beforeSecondSegment = node1
              endSecondSegment = node2
              reverseSecondSegment = swapped
              reverseFirstSegment = swapped


              encodeMove(beforeFirstSegment: Int, endFirstSegment: Int, reverseFirstSegment: Boolean,
                beforeSecondSegment: Int, endSecondSegment: Int, reverseSecondSegment: Boolean)

              if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())) {
                startVehicle = vehicle1 + 1
                return
              }
            }
          }
        }
      }
    }
  }

  override def instantiateCurrentMove(newObj: Int): SegmentExchangeMove = {
    SegmentExchangeMove(
      beforeFirstSegment, endFirstSegment,reverseFirstSegment,
      beforeSecondSegment, endSecondSegment, reverseSecondSegment,
      newObj, this,neighborhoodName)
  }

  def encodeMove(beforeFirstSegment: Int,endFirstSegment: Int, reverseFirstSegment:Boolean,
                 beforeSecondSegment: Int,endSecondSegment: Int,reverseSecondSegment:Boolean) ={
    assert(vrp.routeNr(beforeFirstSegment).value != vrp.routeNr(beforeSecondSegment).value)
    assert(vrp.positionInRoute(beforeFirstSegment).value < vrp.positionInRoute(endFirstSegment).value)
    assert(vrp.positionInRoute(beforeSecondSegment).value < vrp.positionInRoute(endSecondSegment).value)

    val firstSegment = cut(beforeFirstSegment,endFirstSegment)
    val secondSegment = cut(beforeSecondSegment,endSecondSegment)
    val correctedFirstSegment = if(reverseFirstSegment) reverse(firstSegment) else firstSegment
    val correctedSecondSegment = if(reverseSecondSegment) reverse(secondSegment) else secondSegment
    insert(correctedFirstSegment,beforeSecondSegment)
    insert(correctedSecondSegment,beforeFirstSegment)
  }
}

case class SegmentExchangeMove(beforeFirstSegment: Int,endFirstSegment: Int,reverseFirstSegment:Boolean,
                               beforeSecondSegment: Int, endSecondSegment: Int, reverseSecondSegment:Boolean,
                               override val objAfter: Int,override val neighborhood:SegmentExchange,
                               override val neighborhoodName:String = null)
  extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def stablePointsOfImpactedVehicles: List[Int] = List(beforeFirstSegment,beforeSecondSegment)

  override def unroutedPoints: List[Int] = Nil

  // overriding methods
  override def encodeMove() {
    neighborhood.encodeMove(
      beforeFirstSegment,endFirstSegment,reverseFirstSegment,
      beforeSecondSegment,endSecondSegment,reverseSecondSegment)
  }
}
