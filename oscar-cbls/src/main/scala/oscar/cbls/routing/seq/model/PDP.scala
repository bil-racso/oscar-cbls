package oscar.cbls.routing.seq.model

import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.constraints.lib.basic.{EQ, LE}
import oscar.cbls.invariants.core.computation.{CBLSIntVar, FullRange, IntValue, Store}
import oscar.cbls.invariants.lib.routing.{VehicleOfNodes, RouteSuccessorAndPredecessors}
import oscar.cbls.invariants.lib.seq.Precedence
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.{IntVarObjective, CascadingObjective, Objective}

/**
  * Created by fabian on 04-07-16.
  */
class PDP(override val n:Int, override val v:Int, override val m:Store, maxPivotPerValuePercent:Int = 4)
  extends VRP(n,v,m,maxPivotPerValuePercent){

  /**
    * The array that maintains pickup and delivery node.
    * Each value (representing a node) contains the load value of the node
    * (negative value for a delivery node, positive for a pickup node and 0 for a depot)
    * and the value of the related node.
    */
  private val pickupDeliveryNodes:Array[(Int,Int)] = new Array[(Int, Int)](n)
  for(i <- 0 until v)pickupDeliveryNodes(i) = (0,i)

  private val pickupNodes:Array[Int] = new Array[Int]((n-v)/2)
  private val deliveryNodes:Array[Int] = new Array[Int]((n-v)/2)

  val (next,prev) = RouteSuccessorAndPredecessors(routes,v,n)
  val vehicleOfNodes = VehicleOfNodes(routes,v)

  val fastConstraints = new ConstraintSystem(m)
  val slowConstraints = new ConstraintSystem(m)
  var precedenceObj:IntVarObjective = null


  /**
    * Add a new PickupAndDelivery couple and set this two basic constraints :
    * A strong constraint that specified that each pickup node has to be before his related delivery node
    * A strong constraint that specified that each couple of point (pickup and delivery) has to be in the same route.
    *
    * @param p the pickup point
    * @param d the delivery point
    */
  def addPickupDeliveryCouple(p:Int, d:Int, coupleNumber:Int, value:Int = 1): Unit ={
    pickupDeliveryNodes(p) = (value,d)
    pickupDeliveryNodes(d) = (-value,p)
    pickupNodes(coupleNumber) = p
    deliveryNodes(coupleNumber) = d
    fastConstraints.post(EQ(vehicleOfNodes(p),vehicleOfNodes(d)))
  }

  /**
    * Add defined couples.
    * The user has to specify two arrays. One containing the pickup points and
    * another one containing the related delivery points. So pickup(1) is the pickup point of delivery(1).
    *
    * @param pickups the pickup points array
    * @param deliverys the delivery points array
    */
  def addPickupDeliveryCouples(pickups:Array[Int], deliverys:Array[Int]): Unit ={
    assert(pickups.length == deliverys.length,
      "The pickup array and the delivery array must have the same length.")
    assert(!pickups.exists(_ >= n) || !deliverys.exists(_ >= n),
      "The pickup and the delivery array may only contain values between 0 and the number of nodes")
    assert(pickups.intersect(deliverys).length == 0,
      "One node can't be a pickup node and a delivery node at the same time")

    var precedenceList = List.tabulate((n-v)/2)(c => (pickups(c),deliverys(c)))
    precedenceObj = new IntVarObjective(Precedence(routes,precedenceList))
    for(i <- pickups.indices){
      addPickupDeliveryCouple(pickups(i),deliverys(i),i)
    }

  }

  /**
    * This method check if the given node is a pickup one. (if his load value is > 0 it's a pickup node)
    *
    * @param index the index of the node
    * @return
    */
  def isPickup(index:Int): Boolean = pickupDeliveryNodes(index)._1 > 0

  /**
    * This method check if the given node is a delivery one. (if his load value is < 0 it's a delivery node)
    *
    * @param index the index of the node
    * @return
    */
  def isDelivery(index:Int):Boolean = pickupDeliveryNodes(index)._1 < 0

  def getRelatedPickup(d:Int): Int ={
    assert(pickupDeliveryNodes(d)._1 < 0,"This node isn't a delivery node")
    pickupDeliveryNodes(d)._2
  }

  def getRelatedDelivery(p:Int): Int ={
    assert(pickupDeliveryNodes(p)._1 > 0,"This node isn't a pickup node")
    pickupDeliveryNodes(p)._2
  }

  /**
    * @param n the index of a node
    * @return the load value of the node
    */
  def deltaAtNode(n:Int): Int ={
    pickupDeliveryNodes(n)._1
  }

  def getPickups: Iterable[Int] = pickupNodes

  def getDeliverys: Iterable[Int] = deliveryNodes

  def getRoutedPickups: Iterable[Int] = pickupNodes.filter(isRouted(_))

  def getRoutedDeliverys: Iterable[Int] = deliveryNodes.filter(isRouted(_))

  def getRoutedPickupsPredecessors: Iterable[Int] = getRoutedPickups.map(prev(_).value)

  def getRoutedDeliverysPredecessors: Iterable[Int] = getRoutedDeliverys.map(prev(_).value)

  def getUnroutedPickups: Iterable[Int] = pickupNodes.filter(!isRouted(_))

  def getUnroutedDeliverys: Iterable[Int] = deliveryNodes.filter(!isRouted(_))

  def getNodesBeforeRelatedDelivery()(node:Int):Iterable[Int] = {
    assert(isPickup(node), "The referenced node must be a pickup one !")
    getNodesBeforePosition()(getRelatedDelivery(node))
  }

  def getNodesAfterRelatedPickup()(node: Int): Iterable[Int] = {
    assert(isDelivery(node), "The referenced node must be a delivery one !")
    getNodesAfterPosition()(getRelatedPickup(node))
  }

  /**
    * Still Usefull ???
    *
    * This method search all the complete segments contained in a specified route.
    * A segment is considered as complete when you can move it to another branch
    * without breaking the precedence constraint.
    * It runs through the specified route and try to create the smallest complete segments possible
    * After that it try to combine adjacent segment
    *
    * @param routeNumber the number of the route
    * @return the list of all the complete segment present in the route
    */
  def getCompleteSegments(routeNumber:Int): List[(Int,Int)] ={
    val route = getRouteOfVehicle(routeNumber)
    /**
      * Each value of segmentsArray represent a possible complete segment.
      * The Int value represents the amount of pickup nodes whose related delivery node isn't currently in the segment
      * The List[Int] value represents the segment
      */
    val segmentsArray:Array[(Int,List[Int])] = new Array[(Int, List[Int])](route.size)

    for(i <- route.indices){
      if(isPickup(route(i))){
        //If the node is a pickup one, we add the node to all the active segment and we create a new one
        for(j <- segmentsArray.indices){
          if(segmentsArray(j) != null) {
            if (segmentsArray(j)._1 != 0) {
              val currentSegment = segmentsArray(j)._2 :+ route(i)
              val nbOfSinglePickup = segmentsArray(j)._1 + 1
              segmentsArray(j) = (nbOfSinglePickup, currentSegment)
            }
          }
        }
        val nodeList:List[Int] = route(i)::Nil
        segmentsArray(i) = (1,nodeList)
      }
      else{
        for(j <- segmentsArray.indices){
          if(segmentsArray(j) != null) {
            if (segmentsArray(j)._1 != 0) {
              /**
                * If the segment doesn't contain the related pickup node it means that the related pickup node is before
                * the beginning of the segment and thus this is not possible to create a complete segment beginning
                * at this position.
                */
              if (!segmentsArray(j)._2.contains(getRelatedPickup(route(i)))) {
                segmentsArray(j) = null

                /**
                  * Else we decrement the number of single pickup
                  */
              }else {
                val currentSegment = segmentsArray(j)._2 :+ route(i)
                val nbOfSinglePickup = segmentsArray(j)._1 - 1
                segmentsArray(j) = (nbOfSinglePickup, currentSegment)
              }
            }
          }
        }
      }
    }

    var completeSegments: List[(Int, Int)] = Nil

    /**
      * We loop only on the segment that are not null and whose the number of single pickup is equals to 0
      */
    for(i <- segmentsArray.indices)if(segmentsArray(i) != null && segmentsArray(i)._1 == 0){
      val currentSegment = segmentsArray(i)._2
      completeSegments = (currentSegment.head, currentSegment.last) :: completeSegments
      var j = i-1
      var currentPreds = prev.element(route(i)).value
      while(j != -1){
        if(segmentsArray(j) != null && currentPreds == segmentsArray(j)._2.last){
          completeSegments = (segmentsArray(j)._2.head, currentSegment.last) :: completeSegments
          currentPreds = prev.element(route(j)).value
        }
        j -= 1
      }
    }
    completeSegments
  }


  //---- Vehicle capacity ----//

  val defaultArrivalLoadValue = 0

  //We create a array of size v+1 so when we initiate constraints the max capacity for unrouted node is null.
  //Maybe not the best solution but it works.
  val vehicleMaxCapacity:Array[Int] = Array.tabulate(v+1) ((vehicle:Int) => 0)

  val arrivalLoadValue = Array.tabulate(n){ (i:Int) => CBLSIntVar(m, 0, FullRange, "Arrival load at node " + i)}

  var leaveLoadValue:Array[IntValue] = null

  def setArrivalLeaveLoadValue(): Unit ={
    leaveLoadValue = Array.tabulate(n+1) {
      (i :Int) =>
        if(i == n || i < v)
          defaultArrivalLoadValue
        else
          arrivalLoadValue(i) + deltaAtNode(i)
    }
    for(i <- 0 until n){
      arrivalLoadValue(i) <== leaveLoadValue.element(prev(i))
    }
  }

  def setVehiclesMaxCargo(max:Int): Unit ={
    for(i <- 0 until v)
      vehicleMaxCapacity(i) = max
  }

  def setVehiclesCapacityStrongConstraint(): Unit ={
    for(i <- arrivalLoadValue.indices)
      slowConstraints.post(LE(arrivalLoadValue(i), vehicleMaxCapacity.element(vehicleOfNodes(i))))
  }
}
