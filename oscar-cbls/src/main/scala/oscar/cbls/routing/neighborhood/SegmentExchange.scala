package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._
import oscar.cbls.search.algo.{Pairs, HotRestart}
import scala.collection.immutable.{HashMap, SortedMap, SortedSet}

/**
 * swaps segments of different vehicles
 * THIS IS EXPERIMENTAL
 */
case class SegmentExchange(override val vrp: VRP with PositionInRouteAndRouteNr with NodesOfVehicle,
                      relevantNeighbors:()=>Int=>Iterable[Int],
                      vehicles:() => List[Int],
                      neighborhoodName:String = "SegmentExchange",
                      hotRestart:Boolean = true,
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

      //println("checking vehicle" + vehicle1, " others:" + otherVehicles)

      val otherVehiclesSet:SortedSet[Int] = SortedSet.empty[Int] ++ otherVehicles
      val nodesToConsider:SortedSet[Int] = otherVehicles.foldLeft(SortedSet.empty[Int])((acc,vehicle) => acc ++ vrp.getRouteOfVehicle(vehicle))
      val routeOfVehicle1 = vrp.getRouteOfVehicle(vehicle1)
      //println("route of checked vehicle:" + routeOfVehicle1)
      //cluster the closest by the otherVehicles
      val routeWithRelevantNeighborsAndTheirPositionInTheirRouteGroupedByVehicles = routeOfVehicle1.map(node =>
        (node, relevantNeighborsNow(node)
          .filter((node:Int) => node >= vrp.V && nodesToConsider.contains(node))
          .map(node => (node,vrp.positionInRoute(node).value))
          .groupBy(nodeAndPosition => vrp.routeNr(nodeAndPosition._1).value))
      ).filter(_._2.nonEmpty)

      //println("routeWithRelevantNeighborsAndTheirPositionInTheirRouteGroupedByVehicles: " + routeWithRelevantNeighborsAndTheirPositionInTheirRouteGroupedByVehicles)
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
                               override val neighborhoodName:String = "SegmentExchangeMove")
  extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def impactedPoints: List[Int] = List(beforeFirstSegment,endFirstSegment,beforeSecondSegment,endSecondSegment)

  // overriding methods
  override def encodeMove() {
    neighborhood.encodeMove(
      beforeFirstSegment,endFirstSegment,reverseFirstSegment,
      beforeSecondSegment,endSecondSegment,reverseSecondSegment)
  }

  override def toString: String = {
    neighborhoodNameToString + "SegmentExchange(beforeFirstSegment:" + beforeFirstSegment + "; endFirstSegment:" + endFirstSegment + "; reverseFirstSegment:" + reverseFirstSegment +
      "; beforeSecondSegment:" + beforeSecondSegment + "; endSecondSegment:" + endSecondSegment + "; reverseSecondSegment:" + reverseSecondSegment + objToString + ")"
  }
}



case class SegmentExchangePickupAndDelivery(override val vrp: VRP with PositionInRouteAndRouteNr with NodesOfVehicle with PickupAndDeliveryCustomers,
                                            val neighborhoodName:String = "SegmentExchangePickupAndDelivery",
                                            val hotRestart:Boolean = true,
                                            val best:Boolean = false) extends EasyRoutingNeighborhood[SegmentExchangePickupAndDeliveryMove](best,vrp,neighborhoodName) {

  var beforeFirstSegment: Int = 0
  var endFirstSegment: Int = 0
  var reverseFirstSegment:Boolean = false
  var beforeSecondSegment: Int = 0
  var endSecondSegment: Int = 0
  var reverseSecondSegment:Boolean = false
  var startVehicle = 0

  override def exploreNeighborhood(): Unit = {
    var completeSegments:List[List[(Int,Int)]] = Nil
    for(v <- 0 until vrp.V){
      completeSegments = vrp.getCompleteSegments(v) :: completeSegments
    }

    for(route1 <- 0 until vrp.V){
      for(segment1 <- completeSegments(route1)) {
        for (route2 <- route1+1 until vrp.V) {
          for(segment2 <- completeSegments(route2)){
            beforeFirstSegment = vrp.preds(segment1._1).value
            endFirstSegment = segment1._2
            beforeSecondSegment = vrp.preds(segment2._1).value
            endSecondSegment = segment2._2

            encodeMove(beforeFirstSegment,endFirstSegment,reverseFirstSegment,beforeSecondSegment,endSecondSegment,reverseSecondSegment)
            if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())) {
              return
            }
          }
        }
      }
    }
  }

  override def instantiateCurrentMove(newObj: Int): SegmentExchangePickupAndDeliveryMove = {
    SegmentExchangePickupAndDeliveryMove(
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

case class SegmentExchangePickupAndDeliveryMove(beforeFirstSegment: Int,endFirstSegment: Int,reverseFirstSegment:Boolean,
                               beforeSecondSegment: Int, endSecondSegment: Int, reverseSecondSegment:Boolean,
                               override val objAfter: Int,override val neighborhood:SegmentExchangePickupAndDelivery,
                               override val neighborhoodName:String = "SegmentExchangePickupAndDeliveryMove")
  extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def impactedPoints: List[Int] = List(beforeFirstSegment,endFirstSegment,beforeSecondSegment,endSecondSegment)

  // overriding methods
  override def encodeMove() {
    neighborhood.encodeMove(
      beforeFirstSegment,endFirstSegment,reverseFirstSegment,
      beforeSecondSegment,endSecondSegment,reverseSecondSegment)
  }

  override def toString: String = {
    neighborhoodNameToString + "SegmentExchangePickupAndDelivery(beforeFirstSegment:" + beforeFirstSegment + "; endFirstSegment:" + endFirstSegment + "; reverseFirstSegment:" + reverseFirstSegment +
      "; beforeSecondSegment:" + beforeSecondSegment + "; endSecondSegment:" + endSecondSegment + "; reverseSecondSegment:" + reverseSecondSegment + objToString + ")"
  }
}