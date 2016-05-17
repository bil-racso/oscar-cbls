/**
  * *****************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  * ****************************************************************************
  */
package oscar.cbls.routing.model

import java.awt.{Dimension, Toolkit}
import javax.swing.JFrame

import oscar.cbls.constraints.lib.basic.{EQ, LE}
import oscar.cbls.invariants.core.computation.{IntValue, CBLSIntVar, CBLSIntConst, Store}
import oscar.cbls.modeling.Algebra.InstrumentArrayOfIntValue
import oscar.cbls.modeling.Algebra.InstrumentIntVar
import oscar.examples.cbls.routing.visual.ColorGenerator
import oscar.examples.cbls.routing.visual.MatrixMap.{PickupAndDeliveryPoints, RoutingMatrixVisual}
import scala.util.Random

/**
  * The class constructor models a PDP problem with N points (deposits and customers)
  * and V vehicles. It extends the VRP problem
  *
  *
  * @param N the number of points (deposits and customers) in the problem.
  * @param V the number of vehicles.
  * @param m the model.
  * @author fabian.germeau@student.vinci.be
  */
class PDP (N:Int, V:Int, m:Store) extends VRP(N,V,m) with StrongConstraints with PositionInRouteAndRouteNr with Predecessors{
  assert((N-V)%2 == 0,"In order to model a pickup and delivery problem you must have an even number of nodes which are not a depot")

  /**
    * The array that maintains pickup and delivery node.
    * Each value (representing a node) contains the load value of the node
    * (negative value for a delivery node, positive for a pickup node and 0 for a depot)
    * and the value of the related node.
    */
  private val pickupDeliveryNodes:Array[(Int,Int)] = new Array[(Int, Int)](N)
  for(i <- 0 until V)pickupDeliveryNodes(i) = (0,i)

  /**
    * Add a new PickupAndDelivery couple and set this two basic constraints :
    * A strong constraint that specified that each pickup node has to be before his related delivery node
    * A strong constraint that specified that each couple of point (pickup and delivery) has to be in the same route.
    *
    * @param p the pickup point
    * @param d the delivery point
    */
  def addPickupDeliveryCouple(p:Int, d:Int): Unit ={
    pickupDeliveryNodes(p) = (1,d)
    setNodeInformation(p,getNodeInformation(p) + "Pickup node n°" + p + "\n" + "Load value : 1")
    pickupDeliveryNodes(d) = (-1,p)
    setNodeInformation(d,getNodeInformation(d) + "Delivery node n°" + d + "\n" + "Load value : -1")
    strongConstraints.post(LE(positionInRoute(p),positionInRoute(d)))
    strongConstraints.post(EQ(routeNr(p),routeNr(d)))
  }

  //TODO : Move this method in a test file
  /**
    * Add a defined number of random PickupAndDelivery couples.
    * We use the range of nodes to generate them. If we have more than nodes.length/2 couples to add,
    * we re-use the range of nodes. Doing that we are sure that all the nodes will be used
    * (if we have a minimum of nodes.length/2 couples to add)
    *
    * @param numberOfCouple the number of couple to add. Default = the number of nodes divide by 2
    */
  def addRandomPickupDeliveryCouples(numberOfCouple:Int = (N-V)/2): Unit ={
    var tempNodes = nodes.toList.drop(V)
    tempNodes = Random.shuffle(tempNodes)
    for(i <- 0 until numberOfCouple){
      val p = tempNodes.head
      tempNodes = tempNodes.tail

      val d = tempNodes.head
      tempNodes = tempNodes.tail

      addPickupDeliveryCouple(p,d)
    }
  }

  /**
    * Add defined couples.
    * The user has to specify two arrays. One containing the pickup points and
    * another one containing the related delivery points. So pickup(1) is the pickup point of delivery(1).
    *
    * @param pickup the pickup points array
    * @param delivery the delivery points array
    */
  def addPickupDeliveryCouples(pickup:Array[Int],delivery:Array[Int]): Unit ={
    assert(pickup.length == delivery.length,
      "The pickup array and the delivery array must have the same length.")
    assert(!pickup.exists(_ >= N) || !delivery.exists(_ >= N),
      "The pickup and the delivery array may only contain values between 0 and the number of nodes")
    assert(pickup.intersect(delivery).length == 0,
      "One node can't be a pickup node and a delivery node at the same time")

    for(i <- pickup.indices){
      addPickupDeliveryCouple(pickup(i),delivery(i))
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

  /**
    * @param n the index of a node
    * @return the load value of the node
    */
  def getLoadValue(n:Int): IntValue ={
    pickupDeliveryNodes(n)._1
  }

  /**
    * @param d the index of a delivery node
    * @return the index of the related pickup node
    */
  def getRelatedPickup(d:Int): Int ={
    assert(pickupDeliveryNodes(d)._1 < 0,"This node isn't a delivery node")
    pickupDeliveryNodes(d)._2
  }

  /**
    * @param p the index of a pickup node
    * @return the index of the related delivery node
    */
  def getRelatedDelivery(p:Int): Int ={
    assert(pickupDeliveryNodes(p)._1 > 0,"This node isn't a pickup node")
    pickupDeliveryNodes(p)._2
  }

  /**
    * O(N)
    *
    * @return All the pickup nodes
    */
  def getPickups: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 > 0)b :: a else a)

  /**
    * O(N)
    *
    * @return All the routed pickup nodes
    */
  def getRoutedPickups: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 > 0 && isRouted(b))b :: a else a)

  /**
    * O(N)
    *
    * @return The predecessor of each routed pickup node
    */
  def getRoutedPickupsPredecessors: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 > 0 && isRouted(b))preds(b).value :: a else a)

  /**
    * O(N)
    *
    * @return All the unrouted pickup nodes
    */
  def getUnroutedPickups: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 > 0 && !isRouted(b))b :: a else a)

  /**
    * O(N)
    *
    * @return All the delivery nodes
    */
  def getDeliverys: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 < 0)b :: a else a)

  /**
    * O(N)
    *
    * @return All the routed delivery nodes
    */
  def getRoutedDeliverys: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 < 0 && isRouted(b))b :: a else a)

  /**
    * O(N)
    *
    * @return The predecessor of each routed delivery nodes
    */
  def getRoutedDeliverysPredecessors: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 < 0 && isRouted(b))preds(b).value :: a else a)

  /**
    * O(N)
    *
    * @return All the unrouted delivery nodes
    */
  def getUnroutedDeliverys: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 < 0 && !isRouted(b))b :: a else a)

  /**
    * @param node the pickup node
    * @return All the nodes preceding the related delivery node
    */
  def getNodesBeforeRelatedDelivery()(node: Int): Iterable[Int] = {
    assert(isPickup(node), "The referenced node must be a pickup one !")
    val routeOfNode = routeNr(node)
    val resRoute = getRouteOfVehicle(routeOfNode.value)
    resRoute.takeWhile(_ != getRelatedDelivery(node))
  }

  /**
    * @param node the delivery node
    * @return All the nodes following the related pickup node
    */
  def getNodesBeforeRelatedPickup()(node: Int): Iterable[Int] = {
    assert(isDelivery(node), "The referenced node must be a delivery one !")
    val routeOfNode = routeNr(node)
    var resRoute = getRouteOfVehicle(routeOfNode.value)
    resRoute = resRoute.dropWhile(_ != getRelatedPickup(node))
    resRoute.drop(1)
  }

  /**
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

    for(n <- route.indices){
      if(isPickup(route(n))){
        //If the node is a pickup one, we add the node to all the active segment and we create a new one
        for(i <- segmentsArray.indices){
          if(segmentsArray(i) != null) {
            if (segmentsArray(i)._1 != 0) {
              val currentSegment = segmentsArray(i)._2 :+ route(n)
              val nbOfSinglePickup = segmentsArray(i)._1 + 1
              segmentsArray(i) = (nbOfSinglePickup, currentSegment)
            }
          }
        }
        val nodeList:List[Int] = route(n)::Nil
        segmentsArray(n) = (1,nodeList)
      }
      else{
        for(i <- segmentsArray.indices){
          if(segmentsArray(i) != null) {
            if (segmentsArray(i)._1 != 0) {
              /**
                * If the segment doesn't contain the related pickup node it means that the related pickup node is before
                * the beginning of the segment and thus this is not possible to create a complete segment beginning
                * at this position.
                */
              if (!segmentsArray(i)._2.contains(getRelatedPickup(route(n)))) {
                segmentsArray(i) = null

                /**
                  * Else we decrement the number of single pickup
                  */
              }else {
                val currentSegment = segmentsArray(i)._2 :+ route(n)
                val nbOfSinglePickup = segmentsArray(i)._1 - 1
                segmentsArray(i) = (nbOfSinglePickup, currentSegment)
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
      var currentPreds = preds(route(i)).value
      while(j != -1){
        if(segmentsArray(j) != null && currentPreds == segmentsArray(j)._2.last){
          completeSegments = (segmentsArray(j)._2.head, currentSegment.last) :: completeSegments
          currentPreds = preds(route(j)).value
        }
        j -= 1
      }
    }
    completeSegments
  }
}

/**
  * This trait maintains the current carrying capacity of each vehicle of the VRP and all the useful related information
  */
trait VehicleWithCapacity extends PDP{

  val defaultArrivalLoadValue = new CBLSIntConst(0)

  /**
    * The variable that maintains the maximum cargo of each vehicle
    */
  val vehicleMaxCapacity:Array[CBLSIntVar] = Array.tabulate(V) (v => CBLSIntVar(m, 0, 0 to N, "maximum capacity of vehicle " + v))

  /**
    * The variable that maintain the current arrival load of a vehicle at each point
    */
  val arrivalLoadValue = Array.tabulate(N){ (n:Int) => CBLSIntVar(m, 0, 0 to N, "Arrival load at node " + n) }

  /**
    * The variable that maintain the current leave load of a vehicle at each point
    */
  var leaveLoadValue:Array[IntValue] = null

  /**
    * This method set the arrival and leave load value of each point (using invariant)
    */
  def setArrivalLeaveLoadValue(): Unit ={
    leaveLoadValue = Array.tabulate(N+1) {
      (n :Int) =>
        if(n == N || n < V)
          defaultArrivalLoadValue
        else{
          arrivalLoadValue(n) + getLoadValue(n)
        }
    }
    for(n <- 0 until N){
      arrivalLoadValue(n) <== leaveLoadValue.element(preds(n))
    }
  }

  /**
    * Allow client to set the max cargo value of a vehicle
    *
    * @param max the maximum cargo value
    * @param v the vehicle
    */
  def setVehicleMaxCargo(max:Int, v:Int): Unit ={
    vehicleMaxCapacity(v) := max
  }

  /**
    * Allow client to set a max cargo value to all the vehicle
    *
    * @param max the maximum cargo value
    */
  def setVehiclesMaxCargo(max:Int): Unit ={
    for(v <- vehicleMaxCapacity.indices)
      setVehicleMaxCargo(max,v)
  }

  /**
    * This method set a strong constraint on the capacity of each vehicle.
    */
  def setVehiclesCapacityStrongConstraint(): Unit ={
    for(n <- arrivalLoadValue.indices)
      strongConstraints.post(LE(arrivalLoadValue(n),vehicleMaxCapacity(0)))
  }
}

/**
  * This trait add a constraint that ensures that the travel time between a pickup node and his related delivery node
  * is lower than twice the perfect travel time
  */
trait MaxTravelDistancePDConstraint extends PDP with HopDistance with Time with TravelTimeAsFunction{

  def setMaxTravelDistancePDConstraint(){
    for(p <- getPickups) {
      val d = getRelatedDelivery(p)
      strongConstraints.post(LE(arrivalTime(d) - leaveTime(p), 2*travelDurationMatrix.getTravelDuration(p, leaveTime(p).value, d)))
    }
  }
}

/**
  * The purpose of this trait is to add the routing map of the problem.
  */
trait PDPMap extends PDP{
  val rm = new RoutingMatrixVisual("PDP Map",pickupAndDeliveryPoints = true)
  rm.setVRP(this)
  rm.setColorValues(ColorGenerator.generateRandomColors(V))

  //The thread of the routing map, it improves slightly the running time
  val routingMapThread = new Thread(rm,"PDP Map Thread")
  routingMapThread.start()

  val f = new JFrame("Pickup And Delivery - PDP Map")
  f.setSize(Toolkit.getDefaultToolkit.getScreenSize.getWidth.toInt,(11*Toolkit.getDefaultToolkit().getScreenSize().getHeight/12).toInt)
  rm.setPreferredSize(new Dimension(f.getHeight,f.getHeight))
  f.add(rm)
  f.pack()
  f.setVisible(true)

  /**
    * This method initialize the map.
    * @param pointsList The list of the nodes to draw
    * @param mapSize The size of the map (NOT in pixel)
    */
  def setMapInfo(pointsList:Array[(Int,Int)],mapSize:Int): Unit ={
    rm.setMapSize(mapSize)
    rm.setPointsList(pointsList.toList)
    rm.drawPoints()
  }

  /**
    * This method draw the map after each move.
    * BUT you have to call it (use afterMove combinator)
    */
  def drawRoutes(): Unit ={
    rm.setMustRefresh(true,(for(c <- 0 until V)yield getRouteOfVehicle(c)).toList)
  }
}
