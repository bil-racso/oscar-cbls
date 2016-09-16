package oscar.cbls.routing.seq.neighborhood

import oscar.cbls.routing.seq.model.PDP
import oscar.cbls.search.core.EasyNeighborhood

/**
  * Created by f.germeau on 16/09/2016.
  */
case class PickupDeliveryCoupleExchange(vrp: PDP,
                                        neighborhoodName:String = "PickupDeliveryCoupleExchange",
                                        best:Boolean = false,
                                        hotRestart:Boolean = true,
                                        k:Int = 10
                                       )extends EasyNeighborhood[PickupDeliveryCoupleExchangeMove](best,neighborhoodName) {

  /**
    * This variable contains the values used to describe the movement.
    * 0° : The number of the first route used
    * 1° : The pickup node removed from the first route
    * 2° : The delivery node removed from the first route
    * 3° : The insertion position of the pickup node from the second route
    * 4° : The insertion position of the delivery node from the second route
    * 5° : The number of the second route used
    * 6° : The pickup node removed from the second route
    * 7° : The delivery node removed from the second route
    * 8° : The insertion position of the pickup node from the first route
    * 9° : The insertion position of the delivery node from the first route
    */
  val exchange = Array.tabulate(10)(n => 0)

  val seq = vrp.routes

  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {
    val routePositionOfNode = vrp.getRoutePositionOfAllNode
    val closestRoutedNeighboursInTime = vrp.computeClosestNeighborInTimeWithCluster()
    for(r1 <- 0 until vrp.v-1){
      exchange(0) = r1
      for(p1 <- vrp.getRoutedPickups.filter(vrp.getVehicleOfNode(_) == r1)){
        exchange(1) = p1
        exchange(2) = vrp.getRelatedDelivery(p1)
        for(r2 <- r1+1 until vrp.v){
          exchange(5) = r2
          for(p2 <- vrp.getRoutedPickups.filter(vrp.getVehicleOfNode(_) == r2)){
            exchange(6) = p2
            exchange(7) = vrp.getRelatedDelivery(p2)
            val closestNeighborsForP1 = closestRoutedNeighboursInTime(p1).filter(vrp.getVehicleOfNode(_)==r2).take(k)
            val closestNeighborsForP2 = closestRoutedNeighboursInTime(p2).filter(vrp.getVehicleOfNode(_)==r1).take(k)
            for(newPredsP1 <- closestNeighborsForP1 if newPredsP1 != p2 && newPredsP1 != exchange(7)){
              for(newPredsD1 <- vrp.getNodesAfterPosition()(routePositionOfNode(newPredsP1))){
                for(newPredsP2 <- closestNeighborsForP2 if newPredsP2 != p1 && newPredsP2 != exchange(2)){
                  for(newPredsD2 <- vrp.getNodesAfterPosition()(routePositionOfNode(newPredsP2))){
                    exchange(3) = newPredsP1
                    exchange(4) = newPredsD1
                    exchange(8) = newPredsP2
                    exchange(9) = newPredsD2
                    encodeMove()
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  def encodeMove(firstPickupPos: Int, firstDeliveryPos: Int,
                 secondPickupNewPos: Int, secondDeliveryNewPos: Int, firstRoute: Int,
                 secondPickupPos: Int, secondDeliveryPos: Int,
                 firstPickupNewPos: Int, firstDeliveryNewPos: Int, secondRoute: Int) ={
    assert(firstRoute != secondRoute,"The first and the second route selected must be different")
    assert(vrp.getVehicleOfNode(firstPickupPos) == firstRoute, "The first pickup node must be removed from the first route")
    assert(vrp.getVehicleOfNode(firstDeliveryPos) == firstRoute, "The first delivery node must be removed from the first route")
    assert(vrp.getVehicleOfNode(secondPickupNewPos) == firstRoute, "The second pickup insertion location must be on the first route")
    assert(vrp.getVehicleOfNode(secondDeliveryNewPos) == firstRoute, "The second delivery insertion location must be on the first route")
    assert(vrp.getVehicleOfNode(secondPickupPos) == secondRoute, "The second pickup node must be removed from the second route")
    assert(vrp.getVehicleOfNode(secondDeliveryPos) == secondRoute, "The second delivery node must be removed from the second route")
    assert(vrp.getVehicleOfNode(firstPickupNewPos) == secondRoute, "The first pickup insertion location must be on the second route")
    assert(vrp.getVehicleOfNode(firstDeliveryNewPos) == secondRoute, "The first delivery insertion location must be on the second route")

    seq.move(firstPickupPos,firstPickupPos,firstPickupNewPos,false)
    seq.move(firstDeliveryPos,firstDeliveryPos,firstDeliveryNewPos,false)
    seq.move(secondPickupPos,secondPickupPos,secondPickupNewPos,false)
    seq.move(secondDeliveryPos,secondDeliveryPos,secondDeliveryNewPos,false)
  }

  override def instantiateCurrentMove(newObj: Int) =
    PickupDeliveryCoupleExchangeMove(exchange,newObj,this,neighborhoodName)
}

case class PickupDeliveryCoupleExchangeMove(exchange:Array[Int],
                                            override val objAfter: Int, override val neighborhood:PickupDeliveryCoupleExchange,
                                            override val neighborhoodName:String = "PickupDeliveryCoupleExchangeMove"
                                           )extends VRPSMove(objAfter, neighborhood, neighborhoodName, neighborhood.vrp){
  override def impactedPoints: Iterable[Int] = ???

  /** to actually take the move */
  override def commit(): Unit = ???
}