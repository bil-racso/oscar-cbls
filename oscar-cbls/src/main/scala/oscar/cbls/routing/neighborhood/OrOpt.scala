package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model._

/**
  * Created by fabian on 17-03-16.
  */
case class OrOpt(override val vrp: VRP with PositionInRouteAndRouteNr  with PickupAndDeliveryCustomers with HopDistance,
                 potentialInsertionPoints:()=>Iterable[Int] = null,
                 neighborhoodName:String = "OrOpt",
                 best:Boolean = false,
                 hotRestart:Boolean = true) extends EasyRoutingNeighborhood[OrOptMove](best,vrp,neighborhoodName){

  var beforeSegment:Int = 0
  var endSegment:Int = 0
  var insertionPoint:Int = 0
  var reverseSegment:Boolean = false

  override def exploreNeighborhood(): Unit = {
    val completeSegments:Array[List[(Int,Int)]] = new Array[List[(Int, Int)]](vrp.V)
    for(v <- 0 until vrp.V){
      completeSegments(v) = vrp.getCompleteSegments(v)
    }

    for(route1 <- 0 until vrp.V){
      for(segment <- completeSegments(route1)){
        for(route2 <- 0 until vrp.V){
          if(route1 != route2){
            val routeOfVehicule = vrp.getRouteOfVehicle(route2)
            val distances = Array.tabulate(routeOfVehicule.length)(n1 =>
              (routeOfVehicule(n1),vrp.distanceFunction(routeOfVehicule(n1), segment._1) + vrp.distanceFunction(routeOfVehicule(n1), segment._2)))
            distances.sortBy(_._2)
            for(node <- distances){
              beforeSegment = vrp.preds(segment._1).value
              endSegment = segment._2
              insertionPoint = node._1
              reverseSegment = false
              if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjOnEncodedMove())) {
                return
              }
            }
          }
        }
      }
    }
  }

  override def instantiateCurrentMove(newObj: Int): OrOptMove = {
    OrOptMove(beforeSegment,endSegment,insertionPoint,reverseSegment,newObj,this)
  }

  def encodeMove(beforeFirstSegment: Int,endFirstSegment: Int, insertionPoint: Int, reverseSegment: Boolean = false) = {
    assert(vrp.routeNr(beforeFirstSegment).value != vrp.routeNr(insertionPoint).value)
    assert(vrp.positionInRoute(beforeFirstSegment).value < vrp.positionInRoute(endFirstSegment).value)

    val firstSegment = cut(beforeFirstSegment, endFirstSegment)
    val correctedFirstSegment = if (reverseSegment) reverse(firstSegment) else firstSegment
    insert(firstSegment, insertionPoint)
  }
}

case class OrOptMove(beforeStart: Int,
                     segEndPoint: Int,
                     insertionPoint: Int,
                     reverseSegment: Boolean,
                     override val objAfter: Int,
                     override val neighborhood:OrOpt,
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
}

