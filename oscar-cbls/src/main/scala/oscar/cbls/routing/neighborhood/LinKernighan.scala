package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model.{PositionInRouteAndRouteNr, VRP}

/**
  * Created by fabian on 18-04-16.
  */
case class LinKernighan(routeNr:Int,
                        override val vrp: VRP with PositionInRouteAndRouteNr,
                        neighborhoodName: String = "LinKernighan",
                        best: Boolean = false,
                        hotRestart: Boolean = true) extends EasyRoutingNeighborhood[LinKernighanMove](best, vrp, neighborhoodName) {
  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  override def exploreNeighborhood(): Unit = {

  }

  override def instantiateCurrentMove(newObj: Int): LinKernighan = {

  }
}

case class LinKernighanMove(override val objAfter: Int,
                            override val neighborhood: LinKernighan,
                            override val neighborhoodName: String = "LinKernighanMove") extends VRPMove(objAfter, neighborhood, neighborhoodName){
  override def encodeMove(): Unit = ???

  override def impactedPoints: List[Int] = ???
}
