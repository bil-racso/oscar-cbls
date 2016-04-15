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
/**
 * *****************************************************************************
 * Contributors:
 *     This code has been initially developed by De Landtsheer Renaud and Ghilain Florent.
 * ****************************************************************************
 */

package oscar.cbls.routing.model

import oscar.cbls.constraints.lib.basic.{NE, EQ, LE}
import oscar.cbls.invariants.core.algo.heap.{BinomialHeapIterator, BinomialHeap}
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.numeric.{Sum2, Sum}
import oscar.cbls.invariants.lib.set.{SetSum, Cardinality}
import oscar.cbls.modeling.Algebra._
import oscar.cbls.search.algo.{LazyQuicksort, KSmallest}

import scala.collection.immutable.{HashMap, SortedMap, SortedSet}
import scala.math.min
import scala.util.Random

/**
  * The class constructor models a VRP problem with N points (deposits and customers)
  * and V vehicles.
 *
 * Vehicles are supposed to leave from their depot, and come back to it.
 * they all have a different depot (but yo ucan put them at the same place if you want)
 *
 * Info: after instantiation, each customer point is unrouted, and each vehicle loop on his deposit.
 *
 * @param N the number of points (deposits and customers) in the problem.
 * @param V the number of vehicles.
 * @param m the model.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
class VRP(val N: Int, val V: Int, val m: Store) {
  /**
   * the data structure array which maintains the successors.
   * It assumed that the V vehicles are indexed from the point 0 to V-1,
   * like that each vehicle is considered like a deposit. Other indexes
   * are used to modelise customers. Finally the value N is used for unrouted node.
   */
  val next: Array[CBLSIntVar] = Array.tabulate(N)(i =>
    if (i < V) CBLSIntVar(m, i, 0 to N - 1, "next" + i)
    else CBLSIntVar(m, N, 0 to N, "next" + i))

  /**unroutes all points of the VRP*/
  def unroute() {
    for (i <- 0 until V) next(i) := i
    for (i <- V until N) next(i) := N
  }

  /**
   * the range of nodes (customers and deposits including) of the problem.
   */
  val nodes = 0 until N
  /**
   * the range vehicle of the problem.
   */
  val vehicles = 0 until V

  /**
   * Returns if a given point is a depot.
    *
    * @param n the point queried.
   * @return true if the point is a depot, else false.
   */
  def isADepot(n: Int): Boolean = { n < V }

  /**
   * Returns if a given point is still routed.
    *
    * @param n the point queried.
   * @return true if the point is still routed, else false.
   */
  def isRouted(n: Int): Boolean = { next(n).newValue != N }

  /**
   * This function is intended to be used for testing only.
   * setCircuit(List(1,2,3,4)) produces the following route :
   * 1 -> 2 -> 3 -> 4 (-> 1)
   */
  def setCircuit(nodes: List[Int]): Unit = {
    def setCircuit(start: Int, nodes: List[Int]): Unit = {
      nodes match {
        case Nil => next(start) := start
        case List(x) => next(x) := start
        case x :: r => next(x) := r.head; setCircuit(start, r)
      }
    }

    nodes match {
      case Nil => ()
      case x :: r => next(x) := r.head; setCircuit(x, r)
    }
  }

  /**
   * @return the list of unrouted nodes as a String.
   */
  def unroutedToString: String = {
    "unrouted: " + nodes.filterNot(isRouted(_)).toList + "\n"
  }

  /**
   * @return the route of a vehicle as a String.
   */
  def routeToString(vehicle: Int): String = {
    "Vehicle " + vehicle + ": " + getRouteOfVehicle(vehicle).mkString("->")
  }

  /**
   * the route of the vehicle, starting at the vehicle node, and not including the last vehicle node
    *
    * @param vehicle
   * @return
   */
  def getRouteOfVehicle(vehicle:Int):List[Int] = {
    var current = next(vehicle).value
    var acc:List[Int] = List(vehicle)
    while (current != vehicle) {
      acc = current :: acc
      current = next(current).newValue //to avoid unnecessary propagation
    }
    acc.reverse
  }

  /**
   * Redefine the toString method.
    *
    * @return the VRP problem as a String.
   */
  override def toString: String = {
    var toReturn = unroutedToString

    for (v <- 0 to V - 1) {
      toReturn += routeToString(v)
      toReturn += "\n"
    }
    for (additionalStringFunction <- additionalStrings) {
      toReturn += additionalStringFunction() + "\n"
    }
    toReturn
  }

  private var additionalStrings: List[() => String] = List.empty
  def addToStringInfo(a: () => String) {
    additionalStrings = a :: additionalStrings
  }

  private val nodeInformations:Array[String] = Array.tabulate(N)(n => "")
  def setNodeInformation(index:Int, info:String): Unit ={
    assert(index < nodeInformations.length,"SetNodeInformation index to high")
    nodeInformations(index) = info
  }

  def getNodeInformation(index:Int): String ={
    assert(index < nodeInformations.length,"GetNodeInformation index to high")
    nodeInformations(index)
  }
}

/**
 * Maintains the set of routed and unrouted nodes.
 * Info : unrouted nodes are those whose next is N.
 * This trait is abstract, since unrouted can be implemented either stand alone,
 * or as a side effect of other traits
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
abstract trait RoutedAndUnrouted extends VRP {
  /**
   * the data structure set which maintains the routed nodes.
   */
  val routed = Filter(next, _ < N)
  m.registerForPartialPropagation(routed)
  
  val routedNotStartingPoint = routed.minus(SortedSet.empty[Int] ++ (0 to V-1))
  m.registerForPartialPropagation(routedNotStartingPoint)

  /**
   * the data structure set which maintains the unrouted nodes.
   */
  def unrouted: SetValue
}

/**
 * Maintains the set of unrouted nodes.
 * Info : those whose next is N.
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
trait UnroutedImpl extends VRP with RoutedAndUnrouted {
  /**
   * the data structure set which maintains the unrouted nodes.
   */
  final override val unrouted = Filter(next, _ == N)
  m.registerForPartialPropagation(unrouted)
}

/**
 * Maintains and fixes a penalty weight of unrouted nodes.
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
abstract trait PenaltyForUnrouted extends VRP with RoutedAndUnrouted {
  assert(unrouted != null, "you should put the implementation of Unrouted before PenaltyForUnrouted when declaring your model")

  /**
   * the data structure array which maintains penalty of nodes.
   * it is not supposed to be modified after model close, neither controlled by an invariant
   */
  protected val weightUnroutedPenalty = Array.fill(N)(0)
  /**
   * the variable which maintains the sum of penalty of unrouted nodes, thanks to invariant SumElements.
   */
  val unroutedPenalty = CBLSIntVar(m,name="TotalPenaltyForUnroutedNodes")

  /**
   * It allows you to set the penalty of a given point.
    *
    * @param n the point.
   * @param p the penalty.
   */
  @deprecated("not deprecated, just, do not forget to call closeUnroutedPenaltyWeight afgter you are done with penalties","")
  def setUnroutedPenaltyWeight(n: Int, p: Int) { weightUnroutedPenalty(n) = p }

  /**
   * It allows you to set a specific penalty for all points of the VRP.
    *
    * @param p the penalty.
   */
  def setUnroutedPenaltyWeight(p: Int) { weightUnroutedPenalty.indices.foreach(i => weightUnroutedPenalty(i) = p) }

  def closeUnroutedPenaltyWeight(){
    unroutedPenalty <== Sum(weightUnroutedPenalty, unrouted)
  }

}

/**
 * @author renaud.delandtsheer@cetic.be
 */
trait HopClosestNeighbors extends ClosestNeighbors with HopDistance {
  final override protected def getDistance(from: Int, to: Int): Int = getHop(from, to)
}


abstract trait ClosestNeighborsWithPenaltyForUnrouted extends VRP with PenaltyForUnrouted with ClosestNeighbors{

  var closestNeighborsWithPenaltyForUnrouted: Array[Iterable[Int]] = null

  override def computeClosestNeighbors(): Unit = {
    super.computeClosestNeighbors()
    computeClosestNeighborsWithPenalty()
  }

  private def computeClosestNeighborsWithPenalty() = {
    def arrayOfAllNodes = Array.tabulate(N)(node => node)
    closestNeighborsWithPenaltyForUnrouted = Array.tabulate(N)(node =>
      KSmallest.lazySort(arrayOfAllNodes,
        neighbor => (min(getDistance(neighbor, node), getDistance(node, neighbor)) - weightUnroutedPenalty(neighbor))
      ))
  }

  /**
   * Returns the k nearest nodes of a given node.
   * It allows us to add a filter (optional) on the neighbor.
   *
   * Info : it uses the Currying feature.
    *
    * @param k the parameter k.
   * @param filter the filter, should only return unrouted nodes
   * @param node the given node.
   * @return the k nearest neighbor as an iterable list of Int.
   */
  def kNearestWithPenaltyForUnrouted(k: Int, filter: (Int => Boolean) = (_ => true))(node: Int): Iterable[Int] = {
    if (k >= N - 1) return nodes.filter(filter)

    def kNearestAccumulator(sortedNeighbors: Iterator[Int], k: Int, kNearestAcc: List[Int]): List[Int] = {
      require(k >= 0)
      if(k == 0 || !sortedNeighbors.hasNext){
        kNearestAcc.reverse
      }else{
        val neighbor = sortedNeighbors.next()
        if (filter(neighbor))
          kNearestAccumulator(sortedNeighbors, k - 1, neighbor :: kNearestAcc)
        else
          kNearestAccumulator(sortedNeighbors, k, kNearestAcc)
      }
    }

    kNearestAccumulator(closestNeighbors(node).iterator, k, Nil)
  }
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
abstract trait ClosestNeighbors extends VRP {

  protected def getDistance(from: Int, to: Int): Int

  var closestNeighbors: Array[Iterable[Int]] = null

  def computeClosestNeighbors() = {
    def arrayOfAllNodes = Array.tabulate(N)(node => node)
    closestNeighbors = Array.tabulate(N)(node =>
      KSmallest.lazySort(arrayOfAllNodes,
        neighbor => min(getDistance(neighbor, node), getDistance(node, neighbor))
      ))
  }
  /**
   * Filters the node itself and unreachable neighbors.
   */
  def reachableNeigbors(node: Int) =
    nodes.filter((node2: Int) =>
      node != node2
        && (getDistance(node, node2) != Int.MaxValue
          || getDistance(node2, node) != Int.MaxValue)).toList

  /**
   * Returns the k nearest nodes of a given node.
   * It allows us to add a filter (optional) on the neighbor.
   *
   * Info : it uses the Currying feature.
    *
    * @param k the parameter k.
   * @param filter the filter.
   * @param node the given node.
   * @return the k nearest neighbor as an iterable list of Int.
   */
  def kNearest(k: Int, filter: (Int => Boolean) = (_ => true))(node: Int): Iterable[Int] = {
    if (k >= N - 1) return nodes.filter(filter)

    def kNearestAccumulator(sortedNeighbors: Iterator[Int], k: Int, kNearestAcc: List[Int]): List[Int] = {
      require(k >= 0)
      if(k == 0 || !sortedNeighbors.hasNext){
        kNearestAcc.reverse
      }else{
        val neighbor = sortedNeighbors.next()
        if (filter(neighbor))
          kNearestAccumulator(sortedNeighbors, k - 1, neighbor :: kNearestAcc)
        else
          kNearestAccumulator(sortedNeighbors, k, kNearestAcc)
      }
    }

    kNearestAccumulator(closestNeighbors(node).iterator, k, Nil)
  }
}

/**
 * Maintains the hop distance in the VRP, based either on a matrix, or on another mechanism.
 * We consider that a hop distance of Int.MaxVal is unreachable.
 * HopDistance is only handling simple cost functions such as cost matrices
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait HopDistance extends VRP {
  /**
   * the data structure which maintains the current hop distance of each node to reach his successor.
   * Info : the domain max is (Int.MaxValue / N) to avoid problem with domain. (allow us to use sum invariant without
   * throw over flow exception to save the distance of all vehicle).
   */
  var hopDistance: Array[IntValue] = new Array[IntValue](N)

  /**
   * maintains the total distance of all vehicle, linked on the actual next hop of each node.
   */
  val overallDistance = CBLSIntVar(m, name = "overall distance")

  def assignOverallDistance() {
    overallDistance <== Sum(hopDistance)
  }
  /**
   * the function which defines the distance between two points of the VRP.
   */
  var distanceFunction: ((Int, Int) => Int) = null

  /**
   * This method sets the function distance with a distance matrix.
   * If a more complex function is to be used, set a controlling invariant to the hopDistances yourself.
   * It considers distance from a node to itself as zero.
    *
    * @param DistanceMatrix the distance between each point.
   */
  def installCostMatrix(DistanceMatrix: Array[Array[Int]]) {
    distanceFunction = (i: Int, j: Int) => DistanceMatrix(i)(j)
    for (i <- 0 until N) hopDistance(i) = new Int2Int(next(i), j => { if (j != N) DistanceMatrix(i)(j) else 0 })
    assignOverallDistance()
  }

  /**
   * This method sets the distance to use for the hop between points thanks
   * to a given function.
    *
    * @param fun the function which defines the distance between two points.
   */
  def installCostFunction(fun: (Int, Int) => Int) {
    distanceFunction = fun
    for (i <- 0 until N) hopDistance(i) = new Int2Int(next(i), j => fun(i, j))
    assignOverallDistance()
  }

  def installhopDistance(d: Domain = 0 to Int.MaxValue / N) {
    hopDistance = Array.tabulate(N) { (i: Int) => CBLSIntVar(m, 0, 0 to Int.MaxValue / N, "hopDistanceForLeaving" + i) }
    assignOverallDistance()
  }
  /**
   * Returns the distance from a given node (start node) to another given node (end node) of the VRP.
    *
    * @param from the start node
   * @param to the end node
   * @return the distance between the start and end node as an Int.
   */
  def getHop(from: Int, to: Int): Int = distanceFunction(from, to)
}


trait hopDistancePerVehicle extends HopDistance with NodesOfVehicle{
  var hopDistancePerVehicle:Array[IntValue] = null
  def installHopDistancePerVehicle(){
    hopDistancePerVehicle = Array.tabulate(V)(v => Sum(hopDistance,nodesOfVehicle(v)).setName("totalDistance_" + v))
    addToStringInfo(() => "hopDistancePerVehicle:" + hopDistancePerVehicle.mkString(";"))
  }
}

trait hopsPerVehicle extends NodesOfVehicle{
  var hopsPerVehicle:Array[IntValue] = Array.tabulate(V)(v => Cardinality(nodesOfVehicle(v)).setName("totalHops_" + v))
  addToStringInfo(() => "hopsPerVehicle:" + hopsPerVehicle.mkString(";"))
}

/**
 * Maintains the set of nodes reached by each vehicle
  *
  * @author renaud.delandtsheer@cetic.be
 */
trait NodesOfVehicle extends PositionInRouteAndRouteNr with RoutedAndUnrouted {
  val nodesOfVehicle = Cluster.MakeDense(routeNr).clusters
  final override val unrouted = nodesOfVehicle(V)
}

/**
 * Maintains the position of nodes in the routes, the route number of each node,
 * the length of each route and their last node.
 * that these output variables are registered for a grouped partial propagation
 * to ensure some efficiency in the queries proposed by this trait.
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait PositionInRouteAndRouteNr extends VRP {
  /**
   * the invariant Routes.
   */
  val routes = Routes.buildRoutes(next, V)

  /**
   * the position in route of each node as an array of IntVar.
   */
  val positionInRoute = routes.positionInRoute

  /**
   * the route number of each node as an array of IntVar.
   */
  val routeNr = routes.routeNr

  /**
   * the route length of each route as an array of IntVar.
   */
  val routeLength = routes.routeLength

  {
    val allvars = positionInRoute.toList ++ routeNr ++ routeLength
    m.registerForPartialPropagation(allvars: _*)
  }

  /**
   * Tells if twos given nodes form a segment of route of n minimum length.
    *
    * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @param n the minimum length of segment.
   * @return true if "fromNode" to "toNode" forms a segment of route of n minimum length, else false.
   */
  def isAtLeastAsFarAs(fromNode: Int, toNode: Int, n: Int): Boolean = {
    routeNr(fromNode).value == routeNr(toNode).value &&
      positionInRoute(fromNode).value + n <= positionInRoute(toNode).value
  }

  /**
   * Tells if two given nodes form a segment of route of n maximum length.
    *
    * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @param n the maximum length of route.
   * @return true if "fromNode" to "toNode" forms a segment of route of n maximum length, else false.
   */
  def isAtMostAsFarAs(fromNode: Int, toNode: Int, n: Int): Boolean = {
    routeNr(fromNode).value == routeNr(toNode).value &&
      positionInRoute(fromNode).value + n >= positionInRoute(toNode).value
  }

  /**
   * Tells if two given nodes form a segment of route.
    *
    * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @return true if "fromNode" to "toNode" form a segment of route, else false.
   */
  def isASegment(fromNode: Int, toNode: Int): Boolean = {
    isAtLeastAsFarAs(fromNode, toNode, 1)
  }

  /**
   * Tells if a given node is in a segment of route between fromNode and toNode.
    *
    * @param node the given node queried.
   * @param fromNode the start of the segment of route.
   * @param toNode the end of the segment of route.
   * @return true if node is in a segment of route between "fromNode" and "toNode", else false.
   */
  def isBetween(node: Int, fromNode: Int, toNode: Int): Boolean = {
    if (isASegment(fromNode, toNode)) {
      routeNr(fromNode).value == routeNr(node).value &&
        positionInRoute(fromNode).value <= positionInRoute(node).value &&
        positionInRoute(node).value < positionInRoute(toNode).value
    } else false
  }

  /**
   * Tells if two given nodes are on the same route.
   * ( i.e. they have the same route number)
    *
    * @param n the first given node.
   * @param m the second given node.
   */
  def onTheSameRoute(n: Int, m: Int): Boolean = {
    routeNr(n).value == routeNr(m).value
  }

  def onTheSameRouteMultArg(m:Int)(n:Int): Boolean = {
    routeNr(n).value == routeNr(m).value
  }
}

/**
 * Maintains a penalty weight for routes which do not contain task nodes.
 * That is: they only contain the vehicle node.
  *
  * @author yoann.guyot@cetic.be
 */
trait PenaltyForEmptyRoute extends VRP with PositionInRouteAndRouteNr {
  /**
   * The data structure array which maintains route penalty.
   */
  private val emptyRoutePenaltyWeight: Array[CBLSIntVar] =
    Array.tabulate(V)(v =>
      CBLSIntVar(m, 0, FullRange, "penality of vehicule " + v))

  /**
   * The variable which maintains the set of empty routes.
   * (that is: routes containing no other node than the vehicle node)
   */
  val emptyRoutes = Filter(routeLength, _ <= 1)

  /**
   * The variable which maintains the sum of route penalties,
   * thanks to SumElements invariant.
   */
  val emptyRoutePenalty = Sum(emptyRoutePenaltyWeight, emptyRoutes)

  /**
   * Allows client to set the penalty of a given vehicle route.
    *
    * @param n the node.
   * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(n: Int, p: Int) {
    emptyRoutePenaltyWeight(n) := p
  }

  /**
   * Allows client to set a specific penalty for all the VRP routes.
    *
    * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(p: Int) {
    emptyRoutePenaltyWeight.foreach(penalty => penalty := p)
  }
}

trait PenaltyForEmptyRouteWithException extends VRP with NodesOfVehicle {
  /**
   * The data structure array which maintains route penalty.
   */
  private val emptyRoutePenaltyWeight: Array[CBLSIntVar] =
    Array.tabulate(V)(v =>
      CBLSIntVar(m, name = "penality of vehicule " + v))

  val exceptionNodes: CBLSSetVar = new CBLSSetVar(m, SortedSet.empty, 0 until N, "NodesNotToConsiderForEmptyRoutes")

  private val nodesOfRealVehicles = Array.tabulate(V)(nodesOfVehicle)

  /**
   * The variable which maintains the set of empty routes.
   * (that is: routes containing no other node than the vehicle node)
   */
  val emptyRoutes = Filter(nodesOfRealVehicles.map(
    (vehicleNodes: CBLSSetVar) => Cardinality(vehicleNodes minus exceptionNodes)), _ == 1)

  /**
   * The variable which maintains the sum of route penalties,
   * thanks to SumElements invariant.
   */
  val emptyRoutePenalty = Sum(emptyRoutePenaltyWeight, emptyRoutes)

  /**
   * Allows client to set the penalty of a given vehicle route.
    *
    * @param n the node.
   * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(n: Int, p: Int) {
    emptyRoutePenaltyWeight(n) := p
  }

  /**
   * Allows client to set a specific penalty for all the VRP routes.
    *
    * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(p: Int) {
    emptyRoutePenaltyWeight.foreach(penalty => penalty := p)
  }
}

/**
 * This trait maintains the predecessors of each node of the VRP.
 * It uses the Predecessor invariant.
  *
  * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait Predecessors extends VRP {
  /**
   * the data structure array which maintains the predecessors of each node.
   */
  val preds: Array[IntValue] = Predecessor(next, V).preds
}

/**
  * This trait maintains the current carrying capacity of each vehicle of the VRP and all the useful related information
  */
trait VehicleWithCapacity extends VRP with PickupAndDeliveryCustomers{

  /**
    * @return the current load of the vehicle at the specified point
    */
  val currentLoad = (n:Int) => {
    var current = routeNr(n).value
    var currentValue = 0
    if(routeNr(n).value == V)
      0
    else {
      while (next(current).value != n) {
        current = next(current).value
        currentValue += getLoadValue(current)
      }
      currentValue
    }
  }

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
  * This trait add a new feature for each node that's not a depot.
  * Now each node is either a Pickup node or a Delivery node.
  * This trait maintains the list of pickup and delivery nodes and provides several methods to use them.
  */
trait PickupAndDeliveryCustomers extends VRP with StrongConstraints with PositionInRouteAndRouteNr with Predecessors{

  assert((N-V)%2 == 0,"In order to use this trait you must have an even number of nodes which are not a depot")

  /**
    * The array that maintains pickup and delivery node.
    * Each value (representing a node) contains the load value of the node
    * (-1 for a delivery node and 1 for a pickup node)
    * and the value of the related node.
    */
  private val pickupDeliveryNodes:Array[(Int,Int)] = new Array[(Int, Int)](N)
  for(i <- 0 until V)pickupDeliveryNodes(i) = (0,i)

  /**
    * Add a new PickupAndDelivery couple and set this two basic constraints :
    * A strong constraint that specified that each pickup node has to be before his related delivery node
    * A strong constraint that specified that each couple of point (pickup and delivery) has to be in the same route.
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
    * @param index the index of the node
    * @return
    */
  def isPickup(index:Int): Boolean = pickupDeliveryNodes(index)._1 > 0

  /**
    * This method check if the given node is a delivery one. (if his load value is < 0 it's a delivery node)
    * @param index the index of the node
    * @return
    */
  def isDelivery(index:Int):Boolean = pickupDeliveryNodes(index)._1 < 0

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
    * @param n the index of a node
    * @return the load value of the node
    */
  def getLoadValue(n:Int): Int ={
    pickupDeliveryNodes(n)._1
  }

  /**
    * O(N)
    * @return All the pickup nodes
    */
  def getPickups: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 > 0)b :: a else a)

  /**
    * O(N)
    * @return All the routed pickup nodes
    */
  def getRoutedPickups: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 > 0 && isRouted(b))b :: a else a)

  /**
    * O(N)
    * @return The predecessor of each routed pickup node
    */
  def getRoutedPickupsPredecessors: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 > 0 && isRouted(b))preds(b).value :: a else a)

  /**
    * O(N)
    * @return All the unrouted pickup nodes
    */
  def getUnroutedPickups: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 > 0 && !isRouted(b))b :: a else a)

  /**
    * O(N)
    * @return All the delivery nodes
    */
  def getDeliverys: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 < 0)b :: a else a)

  /**
    * O(N)
    * @return All the routed delivery nodes
    */
  def getRoutedDeliverys: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 < 0 && isRouted(b))b :: a else a)

  /**
    * O(N)
    * @return The predecessor of each routed delivery nodes
    */
  def getRoutedDeliverysPredecessors: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 < 0 && isRouted(b))preds(b).value :: a else a)

  /**
    * O(N)
    * @return All the unrouted delivery nodes
    */
  def getUnroutedDeliverys: Iterable[Int] = pickupDeliveryNodes.indices.foldLeft(List[Int]())((a,b) => if(pickupDeliveryNodes(b)._1 < 0 && !isRouted(b))b :: a else a)

  /**
    * This method is mainly used when you want to move a pickup node on his route
    * @param node the pickup node
    * @return All the nodes preceding the related delivery node
    */
  def getAuthorizedInsertionPositionForPickup()(node: Int): Iterable[Int] = {
    assert(isPickup(node), "The referenced node must be a pickup one !")
    val routeOfNode = routeNr(node)
    val resRoute = getRouteOfVehicle(routeOfNode.value)
    resRoute.takeWhile(_ != getRelatedDelivery(node))
  }

  /**
    * This method is mainly used when you want to move a delivery node on his route
    * @param node the delivery node
    * @return All the nodes following the related pickup node
    */
  def getAuthorizedInsertionPositionForDelivery()(node: Int): Iterable[Int] = {
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
    * It runs through the specified route and try to create the first complete segment possible
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
  * This trait is an extension of the PickupAndDeliveryCustomers trait.
  * It adds the concept of time window. Now a pickup time/delivery time is specified for each node
  */
trait PickupAndDeliveryCustomersWithTimeWindow extends TimeWindow with TravelTimeAsFunction with PickupAndDeliveryCustomers {

  def setEndWindow(node:Int, endWindow:Int, startWindow:Int, nodeDuration:Int): Unit ={
    setNodeInformation(node,getNodeInformation(node) + "\nEnd of time window : " + endWindow)
    setEndWindow(node,endWindow)
    super.setNodeDuration(node,nodeDuration,startWindow)
  }

  /**
    * This method set a linear travel time function. The time function is entirely based on the distanceMatrix
    * @param distanceMatrix the distance matrix of the map
    */
  def setLinearTravelTimeFunction(distanceMatrix: Array[Array[Int]]): Unit ={
    val ttf = new TTFMatrix(N,new TTFConst(500))
    for(i <- 0 until N){
      for(j <- 0 until N){
        ttf.setTTF(i,j,new TTFConst(distanceMatrix(i)(j)))
      }
    }
    setTravelTimeFunctions(ttf)
  }

  //TODO : Refactor this method, actually it's quite ugly and not very comprehensive
  /**
    *This method generate the time window for each point of the problem.
    * It ensures that the arrival time of a pickup node is before the arrival time of his related delivery node
    * And it tries to randomises the problem (with all the differents constraints, the problem is often unsolvable)
    */
  def endWindowGenerator(): Unit ={
    val currentArray:Array[Int] = new Array[Int](N-V)
    val randomIncValues:List[Int] = 2::3::4::5::Nil
    val currentSum = (pos:Int) => {
      var res = 0
      for(i <- 0 to pos) res += currentArray(i)
      res
    }

    val currentPickup:Array[Int] = new Array[Int](N-V)
    val currentSumPickup = (pos:Int) => {
      var res = 0
      for(i <- 0 to pos) res += currentPickup(i)
      res
    }
    var currentTimeUnit = 1
    val nodesOrderedByType = getPickups.toArray
    while(currentSum(currentTimeUnit) < N-V){
      val current = currentSum(currentTimeUnit)
      val currentPick = currentSumPickup(currentTimeUnit)
      val nbOfNodeToAdd = if(N - V - (N-V)/2 - currentPick < V) N-currentPick - (N-V)/2 -V else Math.min(N-V - (N-V)/2 -currentPick,Math.random()*(V*currentTimeUnit - current)).toInt
      for(inc <- 0 until nbOfNodeToAdd){
        val deliveryInc = randomIncValues(scala.util.Random.nextInt(4))
        setEndWindow(nodesOrderedByType(currentPick+inc), 500*(currentTimeUnit+1), 500*currentTimeUnit,50)
        setEndWindow(getRelatedDelivery(nodesOrderedByType(currentPick+inc)),500*(currentTimeUnit+5), 500*(currentTimeUnit+deliveryInc),0)
        currentArray(currentTimeUnit+deliveryInc) += 1
      }
      currentArray(currentTimeUnit) += nbOfNodeToAdd
      currentPickup(currentTimeUnit) += nbOfNodeToAdd
      currentTimeUnit += 1
    }
  }

  //TODO : Try to find a better solution than using BinomialHeap and then turning it to a List
  /**
    * This method generate all the nodes whose arrival time is before the specified node
    * It's similar to the getAuthorizedInsertionPositionForPickup/delivery
    * excepts that this one take account of the time window
    * (mainly used during the insertion phase)
    * @param node the index of the node
    * @return
    */
  def getRoutedNodesBeforeTime()(node:Int): Iterable[Int] ={
    val res:BinomialHeap[Int] = new BinomialHeap[Int](a => a,N-V)
    val time = leaveTime(node).value
    for(v <- 0 until V){
      val route = getRouteOfVehicle(v)
      var index = 0
      while(index < route.length && arrivalTimeToNext(route(index)).value <= time){
        res.insert(route(index))
        index += 1
      }
    }
    var resList:List[Int] = Nil
    for(n <- 0 until res.size)resList = res.popFirst() :: resList
    resList.toIterable
  }

  def getRoutedNodesBeforeTimeOfRoute(r:List[Int])(node:Int):Iterable[Int] ={
    val res = getRoutedNodesBeforeTime()(node)
    res.foldLeft(List.empty[Int])((a,b) => if(r.contains(routeNr(node).value))node::a else a)
  }

  def getRoutedNodesBeforeTimeNotSameRoute()(node:Int):Iterable[Int] ={
    val res = getRoutedNodesBeforeTime()(node)
    res.foldLeft(List.empty[Int])((a,b) => if(routeNr(b).value != routeNr(node).value)node::a else a)
  }
}

/**
  * This trait add a constraint that ensures that the travel time between a pickup node and his related delivery node
  * is lower than twice the perfect travel time
  */
trait MaxTravelDistancePDConstraint extends VRP with HopDistance with PickupAndDeliveryCustomersWithTimeWindow{

  def setMaxTravelDistancePDConstraint(){
    for(p <- getPickups) {
      val d = getRelatedDelivery(p)
      strongConstraints.post(LE(arrivalTime(d) - leaveTime(p), 2*travelDurationMatrix.getTravelDuration(p, leaveTime(p).value, d)))
    }
  }
}
