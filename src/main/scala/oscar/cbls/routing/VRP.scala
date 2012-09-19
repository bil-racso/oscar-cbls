package oscar.cbls.routing

/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer
 ******************************************************************************/

import oscar.cbls.invariants.core.computation.{IntSetVar, IntVar, Model}
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.set.TakeAny
import oscar.cbls.invariants.lib.logic.{Filter, IntVar2IntVarFun, Routes, Cluster}
import oscar.cbls.algebra.Algebra._
import oscar.cbls.constraints.core.Constraint
import collection.immutable.SortedMap
import scala.math._

/**
 * @param N the number of points in the problem.
 * @param V the number of vehicles, assumed to start from points 1 to V
 * @param m the model
 */
class VRP(val N: Int, val V: Int, val m: Model) {
  //successors
  val Next: Array[IntVar] = Array.tabulate(N)(i => new IntVar(m, 0, N, 0, "next" + i))

  for(v <- 1 to V){Next(v) := v}

  val Nodes = 1 until N

  override def toString():String = {
    var toreturn = ""
    for ( v <- 1 to V){
      toreturn += "Vehicle" + v + ":"
      var current = Next(v).value
      while(current != v){
        toreturn += " " + current
        current = Next(current).getValue(true)
      }
      toreturn+="\n"
    }
    toreturn
  }

  /**
   * this flips the segment of route from "from" to "to"
   * they are supposed to be related to each other
   */
  def flipSegment(BeforeSegmentStart:Int, SegmentEnd:Int){
    var nodestack:List[Int] = List.empty
    //register the list of nodes
    var current:Int = BeforeSegmentStart
    while(current != SegmentEnd) {
      nodestack = current :: nodestack
      current = Next(current).value
    }
    while(!nodestack.isEmpty){
      Next(current) := nodestack.head
      current = nodestack.head
      nodestack = nodestack.tail
    }
  }

  def moveSegment(BeforeSegmentStart:Int, SegmentEnd:Int,  InsertionPoint:Int){
    val SegmentStart:Int = Next(BeforeSegmentStart).value
    val oldNextOfSegmentEnd:Int = Next(SegmentEnd).value
    val oldNextOfInsertionPoint:Int = Next(InsertionPoint).value

    Next(BeforeSegmentStart) := oldNextOfSegmentEnd
    Next(SegmentEnd) := oldNextOfInsertionPoint
    Next(InsertionPoint) := SegmentStart
  }
}

/**Maintains the set of unrouted nodes, those whose next is zero*/
trait Unrouted extends VRP {
  val Unrouted: IntSetVar = Filter(Next, (next: Int) => next == 0)
}

/**add this trait if you want to maintain the predecessor of each node in the routing.
 * It also maintains the set of unrouted nodes.
 * This other feature is integrated with the mechanics for maintaining the predecessors, so it comes at cost zero.
 */
trait PredAndUnrouted extends Unrouted {
  private val Preds: Array[IntSetVar] = Cluster.MakeDense(Next).clusters
  val Pred: Array[IntVar] = Preds.map((i: IntSetVar) => TakeAny(i, 0).toIntVar)
  override val Unrouted: IntSetVar = Preds(0)
}

/**maintains the position of nodes in the routes and the route number of each node*/
trait PositionInRouteAndRouteNr extends VRP {
  private val routes = Routes.buildRoutes(Next, V)

  val PositionInRoute = routes.PositionInRoute
  val RouteNr = routes.RouteNr

  def isASegment(fromNode:Int,toNode:Int):Boolean = {
    RouteNr(fromNode).value == RouteNr(toNode).value &&
      PositionInRoute(fromNode).value < PositionInRoute(toNode).value
  }

  /**assuming fromNode,toNOde form a segment*/
  def isBetween(node:Int,fromNode:Int,toNode:Int):Boolean = {
    RouteNr(fromNode).value == RouteNr(node).value &&
      PositionInRoute(fromNode).value < PositionInRoute(node).value && //Todo: check this
      PositionInRoute(node).value <= PositionInRoute(node).value
  }
}

/**declares an objective function, attached to the VRP. */
trait ObjectiveFunction extends VRP {
  val objective: IntVar = new IntVar(m, 0, Int.MaxValue, 0, "objective of VRP")
  m.registerForPartialPropagation(objective)
}

/**maintains the hop distance in the VRP, based either on a matrix, or on another mechanism. *
 * We conside rtht a hopdistance of Int.MaxVal is unreacheable
 */
trait HopDistance extends VRP {
  val hopDistance = Array.tabulate(N) {(i:Int) => new IntVar(m, 0, N, 0, "hopDistanceForLeaving" + i)}

  val overallDistance: IntVar = Sum(hopDistance)

  /**This method sets the distance to use for the hop between points.
   * If a more complex function is to be used, set a controlling invariant to the hopDistances yourself
   * @param DistanceMatrix is the distance between each point. All distance involving point 0 must be zero (they are corrected anyway by the engine)
   */
  def installCostMatrix(DistanceMatrix: Array[Array[Int]]) {
    distanceFunction = (i:Int,j:Int) => DistanceMatrix(i)(j)
    for (i <- 0 until N) hopDistance(i) <== IntVar2IntVarFun(Next(i), j => DistanceMatrix(i)(j))
    for (i <- 0 until N) {
      DistanceMatrix(i)(0) = 0
      DistanceMatrix(0)(i) = 0
    }
  }
  def installCostFunction(fun:(Int, Int) => Int){
    distanceFunction = fun
    for (i <- 1 to N) hopDistance(i) <== IntVar2IntVarFun(Next(i), j => fun(i,j))
  }
  
  var distanceFunction:((Int, Int) => Int) = null
  def getHop(from:Int, to:Int):Int = distanceFunction(from,to)
}

/**declares an objective function, attached to the VRP.
 * And maintains it equal to the hop distance in the VRP,
 * based either on a matrix, or on another mechanism.
*/
trait HopDistanceAsObjective extends HopDistance with ObjectiveFunction {
  objective <== overallDistance
}

/**declares an objective function, attached to the VRP.
 * And maintains it equal to an AddedValue plus the hop distance in the VRP,
 * based either on a matrix, or on another mechanism.
*/
trait HopDistanceAndOtherAsObjective extends HopDistance with ObjectiveFunction {
  def recordAddedFunction(AddedValue: IntVar) {
    objective <== overallDistance + AddedValue
  }
}

/**finds the nearest neighbor of each point
 * used by some neigborhood searches
 */
trait ClosestNeigborPoints extends VRP with HopDistance{
  
  private var closestNeigbors:SortedMap[Int, Array[List[Int]]] = SortedMap.empty

  def saveKNearestPoints(k:Int){
    val neighbors = Array.tabulate(N)((node:Int) => computeKNearestNeighbors(node, k))
    closestNeigbors += ((k,neighbors))
  }
  
  def computeKNearestNeighbors(node:Int, k:Int):List[Int] = {
    val reachableneigbors = Nodes.filter((next:Int)
      => node != next && (getHop(node,next)!= Int.MaxValue || getHop(next, node)!= Int.MaxValue))
    //TODO: this is deeply inefficient. use a lazy quicksort instead.
    val sortedneighbors = reachableneigbors.sortBy((neigbor:Int) => min(getHop(neigbor, node),getHop(node,neigbor)))
    sortedneighbors.toList.take(k)
  }

  def getKNearestNeighbors(k:Int, node:Int):Iterable[Int] = {
    if (k >= N-1) return Nodes
    if(!closestNeigbors.isDefinedAt(k)){
      saveKNearestPoints(k:Int)
    }
    closestNeigbors(k)(node)
  }
}
