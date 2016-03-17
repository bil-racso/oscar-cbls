package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model.{ClosestNeighborsWithPenaltyForUnrouted, PickupAndDeliveryCustomers, PositionInRouteAndRouteNr, VRP}

/**
  * Created by fabian on 17-03-16.
  */
/*case class OrOpt(potentialInsertionPoints:()=>Iterable[Int] = null,
                 override val vrp: VRP with PositionInRouteAndRouteNr with ClosestNeighborsWithPenaltyForUnrouted with PickupAndDeliveryCustomers,
                 neighborhoodName:String = "OrOpt",
                 best:Boolean = false,
                 hotRestart:Boolean = true) extends EasyRoutingNeighborhood[OrOptMove](best,vrp,neighborhoodName){
  var completeSegments:List[List[(Int,Int)]] = Nil
  for(v <- 0 until vrp.V){
    completeSegments = vrp.getCompleteSegments(v) :: completeSegments
  }

  for(route1 <- 0 until vrp.V){
    for(segment <- completeSegments(route1)){

    }
  }

}

case class OrOptMove(beforeStart: Int,
                     segEndPoint: Int,
                     insertionPoint: Int,
                     reverseSegment: Boolean,
                     override val objAfter: Int,
                     override val neighborhood:ThreeOpt,
                     override val neighborhoodName:String = "OrOptMove")
  extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def impactedPoints: List[Int] = List(beforeStart,segEndPoint,insertionPoint)

  // overriding methods
  override def encodeMove() {
    neighborhood.encodeMove(beforeStart, segEndPoint, insertionPoint, reverseSegment)
  }

  override def toString: String =
    (neighborhoodNameToString + "OrOpt(beforeSegStart:" + beforeStart
      + "; end:" + segEndPoint
      + "; insertAfter:" + insertionPoint
      + "; reverse:" + reverseSegment + objToString + ")")
}*/
