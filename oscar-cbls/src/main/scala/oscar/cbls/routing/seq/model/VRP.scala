package oscar.cbls.routing.seq.model

/*******************************************************************************
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
  ******************************************************************************/

import oscar.cbls.invariants.core.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.routing.{VehicleOfNodes, RoutingConventionMethods, NodeOfVehicle, ConstantRoutingDistance}
import oscar.cbls.invariants.lib.seq.{Content, Size}
import oscar.cbls.invariants.lib.set.Diff
import oscar.cbls.modeling.Algebra._
import oscar.cbls.objective.Objective
import oscar.cbls.search.algo.KSmallest

import scala.collection.immutable.SortedSet
import scala.math._

/**
 * The class constructor models a VRP problem with N points (deposits and customers)
 * and V vehicles.
 *
 * Vehicles are supposed to leave from their depot, and come back to it.
 * they all have a different depot (but yo ucan put them at the same place if you want)
 *
 * Info: after instantiation, each customer point is unrouted, and each vehicle loop on his deposit.
 * @param n the number of points (deposits and customers) in the problem.
 * @param v the number of vehicles.
 * @param m the model.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
class VRP(val n: Int, val v: Int, val m: Store, maxPivot:Int = 50) {

  val seq = new CBLSSeqVar(m, IntSequence(0 until v), n-1, "routes", maxPivot=maxPivot)

  /**unroutes all points of the VRP*/
  def unroute() {
    seq := IntSequence(0 until v)
  }

  /**
   * the range of nodes (customers and deposits including) of the problem.
   */
  val nodes = 0 until n
  /**
   * the range vehicle of the problem.
   */
  val vehicles = 0 until v

  /**
   * Returns if a given point is a depot.
   * @param n the point queried.
   * @return true if the point is a depot, else false.
   */
  def isADepot(n: Int): Boolean = { n < v }

  /**
   * Returns if a given point is still routed.
   * @param n the point queried.
   * @return true if the point is still routed, else false.
   */
  def isRouted(n: Int): Boolean = {seq.value.contains(n)}

  /**
   * This function is intended to be used for testing only.
   * setCircuit(List(1,2,3,4)) produces the following route :
   * 1 -> 2 -> 3 -> 4 (-> 1)
   */
  def setCircuit(nodes: Iterable[Int]): Unit = {
    seq := IntSequence(nodes)
    for(v <- 0 until v) require(seq.newValue.contains(v))
  }

  /**
   * @return the list of unrouted nodes as a String.
   */
  def unroutedToString: String = {
    "unrouted: " + unroutedNodes.toList + "\n"
  }

  def unroutedNodes:Iterable[Int] = nodes.filterNot(isRouted)

  /**
   * @return the route of a vehicle as a String.
   */
  def routeToString(vehicle: Int): String = {
    "Vehicle " + vehicle + ": " + getRouteOfVehicle(vehicle).mkString("->")
  }

  /**
   * the route of the vehicle, starting at the vehicle node, and not including the last vehicle node
   * @param vehicle
   * @return
   */
  def getRouteOfVehicle(vehicle:Int):List[Int] = {
    require(vehicle < v)
    var currentVExplorer = seq.newValue.explorerAtAnyOccurrence(vehicle).head.next
    var acc:List[Int] = List(vehicle)
    while (currentVExplorer match{
      case Some(x) if x.value >= v =>
        acc = x.value :: acc
        currentVExplorer = x.next
        true
      case _ => false}) {}
    acc.reverse
  }

  def getNodesOfVehicle(vehicle:Int):SortedSet[Int] = SortedSet.empty[Int] ++ getRouteOfVehicle(vehicle)

  /**
   *
   * @param node a node
   * @return the vehicle reachingthe node, v is it is unrouted
   */
  def getVehicleOfNode(node:Int):Int = {
    val routes = seq.value
    routes.positionOfAnyOccurrence(node) match{
      case None => v
      case Some(position) => RoutingConventionMethods.searchVehicleReachingPosition(position,routes,v)
    }
  }


  /**
   * Redefine the toString method.
   * @return the VRP problem as a String.
   */
  override def toString: String = {
    var toReturn = unroutedToString

    for (v <- 0 to v - 1) {
      toReturn += routeToString(v)
      toReturn += "\n"
    }
    for (additionalStringFunction <- additionalStrings) {
      toReturn += additionalStringFunction() + "\n"
    }
    toReturn
  }

  private var additionalStrings: List[() => String] = List.empty
  protected def addToStringInfo(a: () => String) {
    additionalStrings = a :: additionalStrings
  }

  def onTheSameRoute(node1:Int,node2:Int):Boolean = getVehicleOfNode(node1) == getVehicleOfNode(node2)

  def next(node:Int):Int = RoutingConventionMethods.routingSuccVal2Val(node, seq.value, v)
  def prev(node:Int):Int = RoutingConventionMethods.routingPredVal2Val(node, seq.value, v)
}

trait ConstantDistancePerVehicle extends TotalConstantDistance{
  var distancePerVehicle:Array[CBLSIntVar] = null

  override def setSymmetricDistanceMatrix(symmetricDistanceMatrix:Array[Array[Int]]){
    this.distanceMatrix = distanceMatrix
    this.matrixIsSymmetric = true
    require(distancePerVehicle == null)
    distancePerVehicle = ConstantRoutingDistance(seq, v ,false,symmetricDistanceMatrix,true)
    totalDistance = Sum(distancePerVehicle)
  }

  override def setAsymmetricDistanceMatrix(asymetricDistanceMatrix:Array[Array[Int]]){
    this.distanceMatrix = distanceMatrix
    this.matrixIsSymmetric = false
    require(distancePerVehicle == null)
    distancePerVehicle = ConstantRoutingDistance(seq, v ,false,asymetricDistanceMatrix,false)
    totalDistance = Sum(distancePerVehicle)
  }
}

trait TotalConstantDistance extends VRP{
  var totalDistance:IntValue = null
  var distanceMatrix:Array[Array[Int]] = null
  var matrixIsSymmetric = false

  def setDistanceMatrix(distanceMatrix:Array[Array[Int]]){
    if(ConstantRoutingDistance.isDistanceSymmetric(distanceMatrix)){
      setSymmetricDistanceMatrix(distanceMatrix)
    }else{
      setAsymmetricDistanceMatrix(distanceMatrix)
    }
  }

  def setSymmetricDistanceMatrix(symmetricDistanceMatrix:Array[Array[Int]]){
    require(totalDistance == null)
    this.distanceMatrix = distanceMatrix
    this.matrixIsSymmetric = true
    totalDistance = ConstantRoutingDistance(seq, v ,false,symmetricDistanceMatrix,true)(0)
  }

  def setAsymmetricDistanceMatrix(asymetricDistanceMatrix:Array[Array[Int]]){
    require(totalDistance == null)
    this.distanceMatrix = distanceMatrix
    this.matrixIsSymmetric = false
    totalDistance = ConstantRoutingDistance(seq, v ,false,asymetricDistanceMatrix,false)(0)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait VRPObjective extends VRP {

  private val accumulationVariable = CBLSIntVar(m, 0, FullRange, "objective of VRP")
  protected val objectiveFunction = Objective(accumulationVariable)
  private var objectiveFunctionTerms: List[IntValue] = List.empty

  /** adds a term top the objective function*/
  def addObjectiveTerm(o: IntValue) {
    objectiveFunctionTerms = o :: objectiveFunctionTerms
  }

  m.addToCallBeforeClose(() => closeObjectiveFunction)

  /**
   * This finished the accumulation of terms in the objective unction.
   * You should not call this, actually.
   * it is called by the model on close
   */
  def closeObjectiveFunction {
    if (objectiveFunctionTerms.isEmpty) throw new Error("you have set an Objective function to your VRP, but did not specify any term for it, call vrp.addObjectiveTerm, or add an objective trait to your VRP")
    accumulationVariable <== Sum(objectiveFunctionTerms)
  }

  def getObjective: Objective = objectiveFunction

  this.addToStringInfo(()=>""+getObjective)
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
trait ClosestNeighbors extends VRP {

  protected def getDistance(from: Int, to: Int): Int

  var closestNeighbors: Array[Iterable[Int]] = null

  def computeClosestNeighbors() = {
    def arrayOfAllNodes = Array.tabulate(n)(node => node)
    closestNeighbors = Array.tabulate(n)(node =>
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
   * @param k the parameter k.
   * @param filter the filter.
   * @param node the given node.
   * @return the k nearest neighbor as an iterable list of Int.
   */
  def kNearest(k: Int, filter: (Int => Boolean) = _ => true)(node: Int): Iterable[Int] = {
    if (k >= n - 1) return nodes.filter(filter)

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
 * Maintains the set of unrouted nodes.
 * Info : those whose next is N.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
trait Unrouted extends VRP{
  /**
   * the data structure set which maintains the unrouted nodes.
   */
  val unrouted = Diff(CBLSSetConst(SortedSet(nodes:_*)),Content(seq)).setName("unrouted nodes")
  m.registerForPartialPropagation(unrouted)

  override def unroutedNodes : Iterable[Int] = unrouted.value
}

trait AbstractPenaltyForUnrouted extends VRP{
  /**
   * the variable which maintains the sum of penalty of unrouted nodes, thanks to invariant SumElements.
   */
  var unroutedPenalty:ChangingIntValue=null
  addToStringInfo(() => ""+unroutedPenalty)
}

/**
 * Maintains and fixes a penalty weight of unrouted nodes.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
trait DetailedPenaltyForUnrouted extends AbstractPenaltyForUnrouted with Unrouted{
  def setDetailedUnroutedPenaltyWeights(penalties : Array[Int]) {
    require(unroutedPenalty == null)
    unroutedPenalty = Sum(penalties, unrouted).setName("TotalPenaltyForUnroutedNodes (detailed penalties)")
  }
}

trait StandardPenaltyForUnrouted extends AbstractPenaltyForUnrouted {
  def setStandardUnroutedPenaltyWeight(standardWeight:Int){
    require(unroutedPenalty == null)
    unroutedPenalty = (standardWeight * (n - Size(seq))).setName("TotalPenaltyForUnroutedNodes (standard penalties)")
  }
}

trait NodesOfVehicle extends VRP{
  val nodesOfVehicle=NodeOfVehicle(seq,v)

  override def getNodesOfVehicle(vehicle:Int):SortedSet[Int] = nodesOfVehicle(vehicle).value
}

trait VehicleOfNode extends VRP{
  val vehicleOfNode = VehicleOfNodes(seq,v)

  override def getVehicleOfNode(node:Int):Int = vehicleOfNode(node).value

  override def isRouted(node: Int): Boolean = vehicleOfNode(node).value!=v

}


