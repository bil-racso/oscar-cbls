package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._

/**
  * Created by fabian on 12-04-16.
  */
case class PickupDeliveryCoupleExchange(override val vrp: PDP with PositionInRouteAndRouteNr with NodesOfVehicle with PositionInTime,
                                        val neighborhoodName:String = "PickupDeliveryCoupleExchange",
                                        val hotRestart:Boolean = true,
                                        val best:Boolean = false) extends EasyRoutingNeighborhood[PickupDeliveryCoupleExchangeMove](best,vrp,neighborhoodName) {

  var firstPickupRemoved: Int = 0
  var firstDeliveryRemoved: Int = 0
  var beforeSecondPickupInsertion: Int = 0
  var beforeSecondDeliveryInsertion: Int = 0
  var firstRoute: Int = 0
  var secondPickupRemoved: Int = 0
  var secondDeliveryRemoved: Int = 0
  var beforeFirstPickupInsertion: Int = 0
  var beforeFirstDeliveryInsertion: Int = 0
  var secondRoute: Int = 0
  var firstPickupSegment: Segment = null
  var firstDeliverySegment: Segment = null
  var secondPickupSegment: Segment = null
  var secondDeliverySegment: Segment = null


  override def exploreNeighborhood(): Unit = {
    for(route1 <- 0 until vrp.V-1){
      firstRoute = route1
      for(pickup1 <- vrp.getRoutedPickups.filter(vrp.routeNr(_).value == firstRoute)) {
        firstPickupRemoved = pickup1
        firstDeliveryRemoved = vrp.getRelatedDelivery(pickup1)
        for(route2 <- route1+1 until vrp.V) {
          secondRoute = route2
          for(pickup2 <- vrp.getRoutedPickups.filter(vrp.routeNr(_).value == secondRoute)){
            secondPickupRemoved = pickup2
            secondDeliveryRemoved = vrp.getRelatedDelivery(pickup2)
            val filteredSecondRoute = vrp.getRoutedNodesBeforeTimeOfRoute(List(secondRoute))(firstPickupRemoved).filter(!List(secondDeliveryRemoved,secondPickupRemoved).contains(_))
            val filteredFirstRoute = vrp.getRoutedNodesBeforeTimeOfRoute(List(firstRoute))(secondPickupRemoved).filter(!List(firstDeliveryRemoved,firstPickupRemoved).contains(_))
            for(insertPickup1 <- filteredSecondRoute){
              for(insertDelivery1 <- filteredSecondRoute.dropWhile(_ != insertPickup1).drop(1)){
                for(insertPickup2 <- filteredFirstRoute){
                  for(insertDelivery2 <- filteredFirstRoute.dropWhile(_ != insertPickup2).drop(1)){
                    beforeFirstPickupInsertion = insertPickup1
                    beforeFirstDeliveryInsertion = insertDelivery1
                    beforeSecondPickupInsertion = insertPickup2
                    beforeSecondDeliveryInsertion = insertDelivery2
                    encodeMove(firstPickupRemoved,beforeFirstPickupInsertion,
                      firstDeliveryRemoved,beforeFirstDeliveryInsertion,firstRoute,
                      secondPickupRemoved,beforeSecondPickupInsertion,
                      secondDeliveryRemoved,beforeSecondDeliveryInsertion,secondRoute)
                    if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())) {
                      return
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  override def instantiateCurrentMove(newObj: Int): PickupDeliveryCoupleExchangeMove = {
    PickupDeliveryCoupleExchangeMove(
      firstPickupRemoved, beforeFirstPickupInsertion, firstDeliveryRemoved, beforeFirstDeliveryInsertion, firstRoute,
      secondPickupRemoved, beforeSecondPickupInsertion, secondDeliveryRemoved, beforeSecondDeliveryInsertion, secondRoute,
      newObj, this,neighborhoodName)
  }

  def encodeMove(firstPickupRemoved: Int, beforeFirstPickupInsertion: Int,
                          firstDeliveryRemoved: Int, beforeFirstDeliveryInsertion: Int, firstRoute: Int,
                          secondPickupRemoved: Int, beforeSecondPickupInsertion: Int,
                          secondDeliveryRemoved: Int, beforeSecondDeliveryInsertion: Int, secondRoute: Int) ={
    assert(firstRoute != secondRoute,"The first and the second route selected must be different")
    assert(vrp.routeNr(firstPickupRemoved).value == firstRoute, "The first pickup node must be removed from the first route")
    assert(vrp.routeNr(firstDeliveryRemoved).value == firstRoute, "The first delivery node must be removed from the first route")
    assert(vrp.routeNr(secondPickupRemoved).value == secondRoute, "The second pickup node must be removed from the second route")
    assert(vrp.routeNr(secondDeliveryRemoved).value == secondRoute, "The second delivery node must be removed from the second route")
    assert(vrp.routeNr(beforeFirstPickupInsertion).value == secondRoute, "The first pickup insertion location must be on the second route")
    assert(vrp.routeNr(beforeFirstDeliveryInsertion).value == secondRoute, "The first delivery insertion location must be on the second route")
    assert(vrp.routeNr(beforeSecondPickupInsertion).value == firstRoute, "The second pickup insertion location must be on the first route")
    assert(vrp.routeNr(beforeSecondDeliveryInsertion).value == firstRoute, "The second delivery insertion location must be on the first route")

    cutNodeAfter(vrp.preds(firstDeliveryRemoved).value)
    cutNodeAfter(vrp.preds(firstPickupRemoved).value)
    cutNodeAfter(vrp.preds(secondDeliveryRemoved).value)
    cutNodeAfter(vrp.preds(secondPickupRemoved).value)
    insert(new Segment(firstPickupRemoved,firstPickupRemoved),beforeFirstPickupInsertion)
    insert(new Segment(firstDeliveryRemoved,firstDeliveryRemoved),beforeFirstDeliveryInsertion)
    insert(new Segment(secondPickupRemoved,secondPickupRemoved),beforeSecondPickupInsertion)
    insert(new Segment(secondDeliveryRemoved,secondDeliveryRemoved),beforeSecondDeliveryInsertion)
  }
}


case class PickupDeliveryCoupleExchangeMove(firstPickupRemoved: Int, beforeFirstPickupInsertion: Int,
                                            firstDeliveryRemoved: Int, beforeFirstDeliveryInsertion: Int, firstRoute: Int,
                                            secondPickupRemoved: Int, beforeSecondPickupInsertion: Int,
                                            secondDeliveryRemoved: Int, beforeSecondDeliveryInsertion: Int, secondRoute: Int,
                                            override val objAfter: Int, override val neighborhood:PickupDeliveryCoupleExchange,
                                            override val neighborhoodName:String = "PickupDeliveryCoupleExchangeMove")
  extends VRPMove(objAfter, neighborhood, neighborhoodName){

  override def impactedPoints: List[Int] = List(firstPickupRemoved: Int, beforeFirstPickupInsertion: Int,
    firstDeliveryRemoved: Int, beforeFirstDeliveryInsertion: Int, firstRoute: Int,
    secondPickupRemoved: Int, beforeSecondPickupInsertion: Int,
    secondDeliveryRemoved: Int, beforeSecondDeliveryInsertion: Int, secondRoute: Int)

  // overriding methods
  override def encodeMove() {
    neighborhood.encodeMove(
      firstPickupRemoved, beforeFirstPickupInsertion,
      firstDeliveryRemoved, beforeFirstDeliveryInsertion, firstRoute,
      secondPickupRemoved, beforeSecondPickupInsertion,
      secondDeliveryRemoved, beforeSecondDeliveryInsertion, secondRoute)
  }

  override def toString: String = {
    neighborhoodNameToString + "PickupDeliveryCoupleExchange(firstPickup:" + firstPickupRemoved + "; firstDelivery:" + firstDeliveryRemoved + "; beforeFirstPickupInsertion:" + beforeFirstPickupInsertion + "; beforeFirstDeliveryInsertion:" + beforeFirstDeliveryInsertion + "; firstRoute:" + firstRoute +
      "; secondPickup:" + secondPickupRemoved + "; secondDelivery:" + secondDeliveryRemoved + "; beforeSecondPickupInsertion:" + beforeSecondPickupInsertion + "; beforeSecondDeliveryInsertion:" + beforeSecondDeliveryInsertion + "; secondRoute:" + secondRoute + objToString + ")"
  }
}
