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

import collection.immutable.SortedMap
import math._
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.invariants.core.computation.{ CBLSSetVar, Store, CBLSIntVar }
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.core.algo.heap.BinomialHeap
import oscar.cbls.invariants.lib.numeric.SumElements
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.logic.Filter
import oscar.cbls.invariants.lib.logic.Predecessor
import oscar.cbls.invariants.lib.set.Diff

/**
 * The class constructor models a VRP problem with N points (deposits and customers)
 * and V vehicles.
 *
 * Vehicles are supposed to leave from their depot, and come back to it.
 * they all have a different depot (but yo ucan put them at the same place if you want)
 *
 * Info: after instantiation, each customer point is unrouted, and each vehicle loop on his deposit.
 * @param N the number of points (deposits and customers) in the problem.
 * @param V the number of vehicles.
 * @param m the model.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
class VRP(val N: Int, val V: Int, val m: Store) {
  /**
   * the data structure array which maintains the successors.
   * It assumed that the V vehicles are indexed from the point 0 to V-1,
   * like that each vehicle is considered like a deposit. Other indexes
   * are used to modelise customers. Finally the value N is used for unrouted node.
   */
  val next: Array[CBLSIntVar] = Array.tabulate(N)(i =>
    if (i < V) CBLSIntVar(m, V to N - 1, i, "next" + i)
    else CBLSIntVar(m, 0, N, N, "next" + i))

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
  val Vehicles = 0 until V

  /**
   * Returns if a given point is a depot.
   * @param n the point queried.
   * @return true if the point is a depot, else false.
   */
  def isADepot(n: Int): Boolean = { n < V }

  /**
   * Returns if a given point is still routed.
   * @param n the point queried.
   * @return true if the point is still routed, else false.
   */
  def isRouted(n: Int): Boolean = { next(n).value != N }

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
   * Redefine the toString method.
   * @return the VRP problem as a String.
   */
  override def toString: String = {
    var toReturn = "unrouted: " + nodes.filterNot(isRouted(_)).toList + "\n"

    for (v <- 0 to V - 1) {
      toReturn += "Vehicle " + v + ": " + v
      var current = next(v).value
      while (current != v) {
        toReturn += " -> " + current
        current = next(current).getValue(true)
      }
      toReturn += "\n"
    }
    toReturn
  }
}

/** this records touched points when comit with no undo, or when cleaning move*
* @author renaud.delandtsheer@cetic.be
  * THIS IS EXPERIMENTAL
*/
trait HotSpotRecording extends VRP with MoveDescription {

  var hotspotList: List[Int]
  val hotSpotArray: Array[Int] = Array.tabulate(N)(_ => 0)
  var hotSpotValue: Int = 1 //the value for being in the hotspot, smller and you are not hotspotted

  override def commit(recordForUndo: Boolean) {
    if (!recordForUndo) addMoveToHotSpot()
    super.commit(recordForUndo)
  }

  override def cleanRecordedMoves() {
    addMoveToHotSpot()
    super.cleanRecordedMoves()
  }

  def addMoveToHotSpot() {
    for (a: Affect <- affects) {
      for (n: Int <- a) hotSpot(n)
    }
  }

  def hotSpot(n: Int) {
    if (hotSpotArray(n) != hotSpotValue) {
      hotSpotArray(n) = hotSpotValue
      hotspotList = n :: hotspotList
    }
  }

  def hotSpottedNodes(): Iterable[Int] = hotspotList

  def cleanHotSpot() {
    hotSpotValue += 1
    if (hotSpotValue == Int.MaxValue) {
      for (i <- 0 to N - 1) hotSpotArray(i) = 0
      hotSpotValue = 1
    }
  }
}

/**
 * describes moves in a spart way by use of segments
 * @author renaud.delandtsheer@cetic.be
 */
trait MoveDescription extends VRP {
  private var Recording = true //recording ou comitted
  def isRecording = Recording

  protected var affects: List[Affect] = List.empty

  protected def addMove(affect: Affect) {
    require(Recording)
    affects = affect :: affects
  }

  protected abstract class Affect extends Iterable[Int] {
    def comit(): Affect
    def iterator: Iterator[Int] = null
  }

  case class affectFromVariable(variable: CBLSIntVar, takeValueFrom: CBLSIntVar) extends Affect {
    def comit(): Affect = {
      val oldValue = variable.value
      assert(isRouted(takeValueFrom.value), "you cannot take the value of an unrouted variable " + variable + " take value from " + takeValueFrom)
      variable := takeValueFrom.value
      affectFromConst(variable, oldValue)
    }
  }

  case class affectFromConst(variable: CBLSIntVar, takeValue: Int) extends Affect {
    def comit(): Affect = {
      assert(variable.value != N || takeValue != N, "you cannot unroute a node that is already unrouted " + variable)
      val oldValue = variable.value
      variable := takeValue
      affectFromConst(variable, oldValue)
    }
  }

  protected case class Segment(start: Int, end: Int)

  def cut(beforeStart: Int, end: Int): Segment = {
    assert(!this.isInstanceOf[PositionInRouteAndRouteNr]
      || this.asInstanceOf[PositionInRouteAndRouteNr].onTheSameRoute(beforeStart, end))

    addMove(affectFromVariable(next(beforeStart), next(end)))
    Segment(next(beforeStart).value, end)
  }

  def cutNodeAfter(beforeStart: Int): Segment = {
    assert(isRouted(beforeStart), "you cannot cut after an unrouted node " + beforeStart)
    val start = next(beforeStart).value
    addMove(affectFromVariable(next(beforeStart), next(start)))
    Segment(start, start)
  }

  def segmentFromUnrouted(n: Int): Segment = {
    assert(!isRouted(n), "you cannot make a segment from unrouted if node is actually routed " + n)
    Segment(n, n)
  }

  def reverse(s: Segment): Segment = {
    var prev = s.start
    var current: Int = next(prev).value
    while (prev != s.end) {
      addMove(affectFromConst(next(current), prev))
      prev = current
      current = next(current).value
    }
    Segment(s.end, s.start)
  }

  def insert(s: Segment, node: Int) {
    addMove(affectFromVariable(next(s.end), next(node)))
    addMove(affectFromConst(next(node), s.start))
  }

  def append(s: Segment, t: Segment): Segment = {
    addMove(affectFromConst(next(s.end), t.start))
    Segment(s.start, t.end)
  }

  def unroute(s: Segment) {
    def unroute(n: Int) {
      assert(n >= V, "you cannot unroute a depot: (depot=" + n + ")")
      addMove(affectFromConst(next(n), N))
    }
    var current = s.start
    unroute(current)
    while (current != s.end) {
      current = next(current).value
      unroute(current)
    }
  }

  def commit(recordForUndo: Boolean = false) {
    require(Recording, "MoveDescription should be recording now")
    if (recordForUndo) {
      affects = doAllMovesAndReturnRollBack()
      Recording = false
    } else {
      doAllMoves()
      affects = List.empty
    }
  }

  private def doAllMovesAndReturnRollBack(): List[Affect] = {
    var undoList: List[Affect] = List.empty
    def doIt(toDo: List[Affect]) {
      toDo match {
        case head :: tail => {
          doIt(tail)
          undoList = head.comit() :: undoList
        }
        case Nil => ;
      }
    }
    doIt(affects)
    undoList.reverse //TODO: find a better way to get it.
  }

  private def doAllMoves() {
    def doIt(toDo: List[Affect]) {
      toDo match {
        case head :: tail =>
          doIt(tail); head.comit
        case Nil => ;
      }
    }
    doIt(affects)
  }

  def undo() {
    require(!Recording, "MoveDescription should not be recording now")
    Recording = true
    commit(false)
    Recording = true
  }

  def cleanRecordedMoves() {
    affects = List.empty
    Recording = true
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 */
trait MoveDescriptionSmarter extends MoveDescription with Predecessors {
  def cutAt(start: Int, end: Int): Segment = {
    cut(this.preds(start).value, end)
  }

  def cutNode(n: Int): Segment = {
    cut(this.preds(n).value, n)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait VRPObjective extends VRP {

  val objectiveFunction = CBLSIntVar(m, Int.MinValue, Int.MaxValue, 0, "objective of VRP")
  m.registerForPartialPropagation(objectiveFunction)

  private var objectiveFunctionTerms: List[CBLSIntVar] = List.empty

  /** adds a term top the objective function*/
  def addObjectiveTerm(o: CBLSIntVar) {
    objectiveFunctionTerms = o :: objectiveFunctionTerms
  }

  m.addToCallBeforeClose(_ => closeObjectiveFunction)

  /**
   * This finished the accumulation of terms in the objective unction.
   * You should not call this, actually.
   * it is called by the model on close
   */
  def closeObjectiveFunction() {
    objectiveFunction <== Sum(objectiveFunctionTerms)
  }

  def getObjective(): Int = objectiveFunction.value
}

/**
 * Maintains the set of routed and unrouted nodes.
 * Info : unrouted nodes are those whose next is N.
 * This trait is abstract, since unrouted can be implemented either stand alone,
 * or as a side effect of other traits
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
abstract trait RoutedAndUnrouted extends VRP {
  /**
   * the data structure set which maintains the routed nodes.
   */
  val routed: CBLSSetVar = Filter(next, _ < N)
  m.registerForPartialPropagation(routed)

  /**
   * the data structure set which maintains the unrouted nodes.
   */
  def unrouted: CBLSSetVar
}

/**
 * Maintains the set of unrouted nodes.
 * Info : those whose next is N.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
trait UnroutedImpl extends VRP with RoutedAndUnrouted {
  /**
   * the data structure set which maintains the unrouted nodes.
   */
  final override val unrouted: CBLSSetVar = Filter(next, _ == N)
  m.registerForPartialPropagation(unrouted)
}

/**
 * Maintains and fixes a penalty weight of unrouted nodes.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
abstract trait PenaltyForUnrouted extends VRP with RoutedAndUnrouted {
  assert(unrouted != null, "you should put the implementation of Unrouted before PenaltyForUnrouted when declaring your model")

  /**
   * the data structure array which maintains penalty of nodes.
   */
  val weightUnroutedPenalty: Array[CBLSIntVar] = Array.tabulate(N)(i => CBLSIntVar(m, Int.MinValue, Int.MaxValue, 0,
    "penality of node " + i))
  /**
   * the variable which maintains the sum of penalty of unrouted nodes, thanks to invariant SumElements.
   */
  val unroutedPenalty: CBLSIntVar = SumElements(weightUnroutedPenalty, unrouted)

  /**
   * It allows you to set the penalty of a given point.
   * @param n the point.
   * @param p the penalty.
   */
  def setUnroutedPenaltyWeight(n: Int, p: Int) { weightUnroutedPenalty(n) := p }

  /**
   * It allows you to set a specific penalty for all points of the VRP.
   * @param p the penlaty.
   */
  def setUnroutedPenaltyWeight(p: Int) { weightUnroutedPenalty.foreach(penalty => penalty := p) }
}


/**
 * @author renaud.delandtsheer@cetic.be
 */
trait HopClosestNeighbors extends ClosestNeighbors with HopDistance {
  final override protected def getDistance(from: Int, to: Int): Int = getHop(from, to)
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
abstract trait ClosestNeighbors extends VRP {

  protected def getDistance(from: Int, to: Int): Int
  /**
   * the data structure which maintains the k closest neighbors of each point.
   */
  var closestNeighbors: SortedMap[Int, Array[List[Int]]] = SortedMap.empty

  /**
   * Save the k nearest neighbors of each node of the VRP.
   * It allows us to add a filter (optional) on the neighbor we want to save.
   * @param k the parameter k.
   * @param filter the filter
   */
  def saveKNearestPoints(k: Int, filter: (Int => Boolean) = (_ => true)) {
    if (k < N - 1) {
      val neighbors = Array.tabulate(N)((node: Int) => computeKNearestNeighbors(node, k, filter))
      closestNeighbors += ((k, neighbors))
    }
  }

  /**
   * Computes and returns the k nearest neighbor of a given node.
   * It allows us to add a filter (optional) on the neighbor we want to save.
   * @param node the given node.
   * @param k the parameter k.
   * @param filter the optional filter.
   * @return the k nearest neighbor of the a node as a list of Int.
   */
  def computeKNearestNeighbors(node: Int, k: Int, filter: (Int => Boolean) = (_ => true)): List[Int] = {

    val reachableneigbors = nodes.filter((next: Int) => node != next && filter(next) && (getDistance(node, next) != Int.MaxValue || getDistance(next, node) != Int.MaxValue))

    val heap = new BinomialHeap[(Int, Int)](-_._2, k + 1)

    for (neigbor <- reachableneigbors) {
      heap.insert(neigbor, min(getDistance(neigbor, node), getDistance(node, neigbor)))
      if (heap.size > k) heap.popFirst()
    }

    heap.toList.map(_._1)
  }

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
  def getKNearest(k: Int, filter: (Int => Boolean) = (_ => true))(node: Int): Iterable[Int] = {
    if (k >= N - 1) return nodes
    if (!closestNeighbors.isDefinedAt(k)) {
      saveKNearestPoints(k: Int, filter)
    }
    closestNeighbors(k)(node)
  }
}

/**
 * Maintains the hop distance in the VRP, based either on a matrix, or on another mechanism.
 * We consider that a hop distance of Int.MaxVal is unreachable.
 * HopDistance is only handling simple cost functions such as cost matrices
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
trait HopDistance extends VRP {
  /**
   * the data structure which maintains the current hop distance of each node to reach his successor.
   * Info : the domain max is (Int.MaxValue / N) to avoid problem with domain. (allow us to use sum invariant without
   * throw over flow exception to save the distance of all vehicle).
   */
  val hopDistance = Array.tabulate(N) { (i: Int) => CBLSIntVar(m, 0, Int.MaxValue / N, 0, "hopDistanceForLeaving" + i) }

  /**
   * maintains the total distance of all vehicle, linked on the actual next hop of each node.
   */
  val overallDistance: CBLSIntVar = Sum(hopDistance)

  /**
   * the function which defines the distance between two points of the VRP.
   */
  var distanceFunction: ((Int, Int) => Int) = null

  /**
   * This method sets the function distance with a distance matrix.
   * If a more complex function is to be used, set a controlling invariant to the hopDistances yourself.
   * It considers distance from a node to itself as zero.
   * @param DistanceMatrix the distance between each point.
   */
  def installCostMatrix(DistanceMatrix: Array[Array[Int]]) {
    distanceFunction = (i: Int, j: Int) => DistanceMatrix(i)(j)
    for (i <- 0 until N) hopDistance(i) <== new Int2Int(next(i), j => { if (j != N) DistanceMatrix(i)(j) else 0 })
  }

  /**
   * This method sets the distance to use for the hop between points thanks
   * to a given function.
   * @param fun the function which defines the distance between two points.
   */
  def installCostFunction(fun: (Int, Int) => Int) {
    distanceFunction = fun
    for (i <- 0 until N) hopDistance(i) <== new Int2Int(next(i), j => fun(i, j))
  }

  /**
   * Returns the distance from a given node (start node) to another given node (end node) of the VRP.
   * @param from the start node
   * @param to the end node
   * @return the distance between the start and end node as an Int.
   */
  def getHop(from: Int, to: Int): Int = distanceFunction(from, to)
}

/**
 * Declares an objective function, attached to the VRP.
 * It maintains it equal to the hop distance in the VRP,
 * based either on a matrix, or on another mechanism defined by the distance function.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
trait HopDistanceAsObjectiveTerm extends VRPObjective with HopDistance {
  addObjectiveTerm(overallDistance)
}

/**
 * Maintains the set of nodes reached by each vehicle
 * @author renaud.delandtsheer@cetic.be
 * */
trait NodesOfVehicle extends PositionInRouteAndRouteNr with RoutedAndUnrouted {
  val NodesOfVehicle = Cluster.MakeDense(routeNr).clusters
  final override val unrouted = NodesOfVehicle(V)
}

/**
 * Maintains the position of nodes in the routes, the route number of each node,
 * the length of each route and their last node.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
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

  /**
   * Tells if twos given nodes form a segment of route of n minimum length.
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
   * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @return true if "fromNode" to "toNode" form a segment of route, else false.
   */
  def isASegment(fromNode: Int, toNode: Int): Boolean = {
    isAtLeastAsFarAs(fromNode, toNode, 1)
  }

  /**
   * Tells if a given node is in a segment of route between fromNode and toNode.
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
   * @param n the first given node.
   * @param m the second given node.
   */
  def onTheSameRoute(n: Int, m: Int): Boolean = {
    routeNr(n).value == routeNr(m).value
  }
}

/**
 * Maintains a penalty weight for routes which do not contain task nodes.
 * That is: they only contain the vehicle node.
 * @author yoann.guyot@cetic.be
 * */
trait PenaltyForEmptyRoute extends VRP with PositionInRouteAndRouteNr {
  /**
   * The data structure array which maintains route penalty.
   */
  private val emptyRoutePenaltyWeight: Array[CBLSIntVar] =
    Array.tabulate(V)(v =>
      CBLSIntVar(m, Int.MinValue, Int.MaxValue, 0, "penality of vehicule " + v))

  /**
   * The variable which maintains the set of empty routes.
   * (that is: routes containing no other node than the vehicle node)
   */
  val emptyRoutes = Filter(routeLength, _ <= (1))

  /**
   * The variable which maintains the sum of route penalties,
   * thanks to SumElements invariant.
   */
  val emptyRoutePenalty = SumElements(emptyRoutePenaltyWeight, emptyRoutes)

  /**
   * Allows client to set the penalty of a given vehicle route.
   * @param n the node.
   * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(n: Int, p: Int) {
    emptyRoutePenaltyWeight(n) := p
  }

  /**
   * Allows client to set a specific penalty for all the VRP routes.
   * @param p the penalty.
   */
  def setEmptyRoutePenaltyWeight(p: Int) {
    emptyRoutePenaltyWeight.foreach(penalty => penalty := p)
  }
}

/**
 * This trait maintains the predecessors of each node of the VRP.
 * It uses the Predecessor invariant.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
trait Predecessors extends VRP {
  /**
   * the data structure array which maintains the predecessors of each node.
   */
  val preds: Array[CBLSIntVar] = Predecessor(next, V).preds

  //TODO: ajouter des moves plus simples, sans les neouds sprécédesseurs à chaque fois
}

/**
 * This trait maintains strong constraints system.
 * It redefines the propagation method of ObjectiveFunction trait,
 * that saves time by propagating partially.
 * @author renaud.delandtsheer@cetic.be
 * */
trait StrongConstraints extends VRPObjective {
  /**
   * the strong constraints system.
   */
  var strongConstraints = ConstraintSystem(m)

  override def getObjective(): Int =
    (if (!strongConstraints.isTrue) Int.MaxValue else objectiveFunction.value)
}

/**
 * This trait maintains weak constraints system.
 * @author renaud.delandtsheer@cetic.be
 * */
trait WeakConstraints extends VRPObjective {
  /**
   * the weak constraints system.
   */
  val weakConstraints = ConstraintSystem(m)

  this.addObjectiveTerm(weakConstraints)
}
