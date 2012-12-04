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
  *     This code has been initially developed by De Landtsheer Renaud and Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.model
import collection.immutable.SortedMap
import math._
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.invariants.core.computation.{IntSetVar, Model, IntVar}
import oscar.cbls.invariants.lib.logic.{Filter, Predecessor, Routes, IntVar2IntVarFun}
import oscar.cbls.invariants.lib.numeric.{SumElements, Sum}
import oscar.cbls.objective.ObjectiveTrait
import oscar.cbls.algebra.Algebra._


/**
 * The class constructor models a VRP problem with N points (deposits and customers)
 * and V vehicles. It must be attached to a model.
 *
 * Info: after instantiation, each customer point is unrouted, and each vehicle loop on his deposit.
 * @param N the number of points (deposits and customers) in the problem.
 * @param V the number of vehicles.
 * @param m the model.
 */
class VRP(val N: Int, val V: Int, val m: Model) {
  /**
   * the data structure array which maintains the successors.
   * It assumed that the V vehicles are indexed from the point 0 to V-1,
   * like that each vehicle is considered like a deposit. Other indexes
   * are used to modelise customers. Finally the value N is used for unrouted node.
   */
  val Next: Array[IntVar] = Array.tabulate(N)(i => if(i<V) new IntVar(m, i, N-1, i, "next" + i)
    else new IntVar(m, 0, N, N, "next" + i))

  /**
   * the range of nodes (customers and deposits including) of the problem.
   */
  val Nodes = 0 until N
  /**
   * the range vehicle of the problem.
   */
  val Vehicles = 0 until V

  /**
   * Returns if a given point is a depot.
   * @param n the point queried.
   * @return true if the point is a depot, else false.
   */
  def isADepot(n:Int):Boolean = { n<V }

  /**
   * Returns if a given point is still routed.
   * @param n the point queried.
   * @return true if the point is still routed, else false.
   */
  def isRouted(n:Int):Boolean = {Next(n).value != N}


  /**
   * Returns the list of variables to update with theirs new values in order to reverse
   * a segment of route.
   * @param from the start of segment to reverse.
   * @param to the end of segment to reverse.
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def reverse(from:Int, to:Int):List[(IntVar,Int)]={
    var listToUpdate:List[(IntVar,Int)] = List.empty
    var nodeStack:List[Int] = List.empty
    var current:Int = from
    while(current != to){
      nodeStack = current :: nodeStack
      current = Next(current).value
    }
    while(!nodeStack.isEmpty){
      listToUpdate =(Next(current),nodeStack.head)::listToUpdate
      current = nodeStack.head
      nodeStack = nodeStack.tail
    }
    listToUpdate
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * move a segment and reinsert it after a given point.
   * @param beforeSegmentStart the predecessor of the start of segment.
   * @param segmentEnd the end of segment.
   * @param insertionPoint the point after which to insert the segment.
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
   def moveTo(beforeSegmentStart:Int, segmentEnd:Int,  insertionPoint:Int):List[(IntVar,Int)] = {
    assert(isRouted(insertionPoint))
    val segmentStart:Int = Next(beforeSegmentStart).getValue(true)
    val oldNextOfSegmentEnd:Int = Next(segmentEnd).getValue(true)
    val oldNextOfbeforeSecondSwapPoint:Int = Next(insertionPoint).getValue(true)

    (Next(beforeSegmentStart),oldNextOfSegmentEnd)::(Next(segmentEnd),oldNextOfbeforeSecondSwapPoint)::
      (Next(insertionPoint),segmentStart)::List.empty
  }

  /** Returns the list of variables to update with theirs new values in order to
    * remove points or points of segments of a route. The points or segment's points to remove
    * are given in an iterable list of tuple (first,second) of Integer. The first Integer
    * is the predecessor of the first point to remove, and the second is the last
    * point to remove. This assumes that (first,second) is a segment, and
    * the segments formed by the tuples of the list must be disjoint.
    *
    * Info: remove points of non disjoints segments throw an Exception (ArrayIndexOutOfBounds)
    * you must then do the union of disjoints segments and remove the result.
    * @param l iterable list of tuple of Integer.
    * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
    */
  def remove(l:Iterable[(Int,Int)]):List[(IntVar,Int)]={
    l.foldLeft(List.empty[(IntVar,Int)])((acc:List[(IntVar,Int)],prec:(Int,Int)) =>
    {
      val beforeStart = prec._1
      val end = prec._2

      val insertion = Next(end).value
      var list = (Next(beforeStart),insertion)::(Next(end),N) ::acc
      var start = Next(beforeStart).value
      while(start != end) {
        list = (Next(start),N)::list
        start = Next(start).value
      }
      list
    })
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * add an unrouted point in a route at a given insertion point.
   * Assumes that the point is an unrouted one.
   * @param insertion the place where insert the unrouted node.
   * @param point the unrouted node.
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def add(insertion:Int,point:Int):Iterable[(IntVar,Int)] = {
    assert(!isRouted(point))
    val next = Next(insertion).value
    List((Next(insertion),point),(Next(point),next))
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * update routes by swapping two nodes, a and b.
   * @param before_a the node before node "a".
   * @param a the node which one swap with node "b".
   * @param before_b the node before node "b".
   * @param b the node which one swap with node "a".
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
   def swap(before_a:Int,a:Int,before_b:Int,b:Int):List[(IntVar,Int)] = {
    assert(Next(before_a).value == a && Next(before_b).value == b)
    assert(before_a != before_b && isRouted(before_a) && isRouted(before_b)
      && isRouted(a) && isRouted(b))
    val next_a:Int = Next(a).value
    val next_b:Int = Next(b).value
    (Next(before_a),b)::(Next(b),next_a)::(Next(before_b),a)::(Next(a),next_b)::List.empty
   }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * update a route by replacing the edges (a,b) and (c,d) by the
   * edges (a,c) and (b,d), and reverse the route's segment [b;c].
   * This assumes that "b" is the successor of "a" and d is the successor of "c".
   *
   * Info: this is a 2-OPT move.
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def flip(a:Int,b:Int,c:Int,d:Int):List[(IntVar,Int)] = {
    assert(c != b) // else useless to flip
    (Next(a),c)::(Next(b),d)::reverse(b,c)
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * perform a 2-opt move. A 2-opt move is in fact a flip. More info in comments of flip method.
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def twoOpt(a:Int,b:Int,c:Int,d:Int):List[(IntVar,Int)] = flip(a,b,c,d)


  /**
   * Returns the list of variables to update with theirs new values in order to
   * update a route by replacing the edges (a,b), (c,d) and (e,f) by the
   * edges (a,d), (e,b), and (c,f).
   * This assumes that "b" is the successor of "a", d is the successor of "c",
   * "f" is the successor of "e" and the order of segment's route
   * is fixed as (a,b) appears first, (c,d) appears second, and finally meets the segment (e,f) in the tour.
   *
   * Info : this is a 3-OPT move without reverse.
   * It can also be seen as the movement of a segment (here | bc |) to a given insertion point (here e).
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @param e the start of edge (e,f).
   * @param f the end of edge (e,f).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def threeOptA(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    moveTo(c,e,a)
  }

/**
 * Returns the list of variables to update with theirs new values in order to
 * update a route by replacing the edges (a,b), (c,d) and (e,f) by the
 * edges (a,d), (e,c),(b,f), and reverse the route's segment [b;c].
 * This assumes that "b" is the successor of "a", d is the successor of "c",
 * "f" is the successor of "e" and the order of segment's route
 * is fixed as (a,b) appears first, (c,d) appears second, and finally meets the segment (e,f) in the tour.
 *
 * Info : this is a 3-OPT move with one reverse segment.
 * @param a the start of edge (a,b).
 * @param b the end of edge (a,b).
 * @param c the start of edge (c,d).
 * @param d the end of edge (c,d).
 * @param e the start of edge (e,f).
 * @param f the end of edge (e,f).
 * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
 */
  def threeOptB(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    var listToUpdate:List[(IntVar,Int)] = List.empty
    listToUpdate = (Next(a),d)::listToUpdate
    listToUpdate = (Next(e),c)::listToUpdate
    listToUpdate = reverse(b,c):::listToUpdate
    listToUpdate = (Next(b),f)::listToUpdate
    listToUpdate
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * update a route by replacing the edges (a,b), (c,d) and (e,f) by the
   * edges (a,c), (b,e),(d,f), and reverse the route's segment [b;c] and [e;d]
   * This assumes that "b" is the successor of "a", d is the successor of "c",
   * f is the successor of "e" and the order of segment's route
   * is fixed as (a,b) appears first, (c,d) appears second, and finally meets the segment (e,f) in the tour.
   *
   * Info : this is a 3-OPT move with 2 reverses segments.
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @param e the start of edge (e,f).
   * @param f the end of edge (e,f).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def threeOptC(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    // using two successive flips
    flip(a,b,c,d):::flip(b,d,e,f)
  }


  /**
   * Redefine the toString method.
   * @return the VRP problem as a String.
   */
  override def toString():String = {
    var toReturn = ""
    for ( v <- 0 until V){
      toReturn += "Vehicle" + v + ":"
      var current = Next(v).value
      while(current != v){
        toReturn += " -> " + current
        current = Next(current).getValue(true)
      }
      toReturn+="\n"
    }
    toReturn
  }
}

/**
 * Maintains a integer weight on each node to help to form constraints (adding information).
 */
trait WeightedNode extends VRP {
  /**
   * the data structure array which maintains weights.
   */
  val weightNode : Array[IntVar] = Array.tabulate(N)(i => new IntVar(m, Int.MinValue, Int.MaxValue, 0,
    "weight of node " + i))

  /**
   * It allows you to set the weight of a given point.
   * @param n the point.
   * @param w the weight.
   */
  def fixWeightNode(n:Int,w:Int) { weightNode(n) := w}

  /**
   * It allows you to set a specific weight for all points of the VRP.
   * @param w the weight.
   */
  def fixWeightNode(w:Int) {weightNode.foreach(p => p := w)}
}

/**
 * Maintains and fixes a penalty weight of unrouted nodes.
 */
trait PenaltyForUnrouted extends Unrouted {
  /**
   * the data structure array which maintains penalty of nodes.
   */
  val weightUnroutedPenalty : Array[IntVar] = Array.tabulate(N)(i => new IntVar(m, Int.MinValue, Int.MaxValue, 0,
    "penality of node " + i))
  /**
   * the variable which maintains the sum of penalty of unrouted nodes, thanks to invariant SumElements.
   */
  val UnroutedPenalty : IntVar = SumElements(weightUnroutedPenalty,Unrouted)

  /**
   * It allows you to set the penalty of a given point.
   * @param n the point.
   * @param p the penalty.
   */
  def fixUnroutedPenaltyWeight(n:Int,p:Int) { weightUnroutedPenalty(n) := p}

  /**
   * It allows you to set a specific penalty for all points of the VRP.
   * @param p the penlaty.
   */
  def fixUnroutedPenaltyWeight(p:Int) {weightUnroutedPenalty.foreach(penalty => penalty := p)}
}

/**
 * Maintains the set of unrouted nodes.
 * Info : those whose next is N.
*/
trait Unrouted extends VRP {
  /**
   * the data structure set which maintains the unrouted node.
   */
  val Unrouted: IntSetVar = Filter(Next, (next: Int) => next == N)
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
 */
trait ClosestNeighborPoints extends VRP with HopDistance{
  /**
   * the data structure which maintains the k closest neighbors of each point.
   */
  var closestNeighbors:SortedMap[Int, Array[List[Int]]] = SortedMap.empty
  /**
   * the max average of unrouted point in the k closest neighbors of each point.
   */
  var maxAvgUnrouted:Double = 0

  /**
   * Save the k nearest neighbors of each node of the VRP.
   * It allows us to add a filter (optional) on the neighbor we want to save.
   * @param k the parameter k.
   * @param filter the filter
   */
  def saveKNearestPoints(k:Int,filter:(Int => Boolean) = ( _ => true)){
    if (k < N-1){
      val neighbors = Array.tabulate(N)((node:Int) => computeKNearestNeighbors(node, k,filter))
      closestNeighbors += ((k,neighbors))
    }
  }

  /**
   * Computes and returns in an ordered form (nearest to farthest) the neighbors of a given node.
   * @param node the given node.
   * @return the neighbors of a given node ordered from the nearest to the farthest as a list of Int.
   */
  def computeNearestNeighbors(node:Int):List[Int] = {
    val reachableneigbors = Nodes.filter((next:Int)
      => node != next && (getHop(node,next)!= Int.MaxValue || getHop(next, node)!= Int.MaxValue))
    //TODO: this is deeply inefficient. use a lazy quicksort instead, orr a partial sort based on a heap?
    reachableneigbors.sortBy((neigbor:Int) => min(getHop(neigbor, node),getHop(node,neigbor))).toList
  }

  /**
   * Computes and returns the k nearest neighbor of a given node.
   * It allows us to add a filter (optional) on the neighbor we want to save.
   * @param node the given node.
   * @param k the parameter k.
   * @param filter the optional filter.
   * @return the k nearest neighbor of the a node as a list of Int.
   */
  def computeKNearestNeighbors(node:Int,k:Int,filter:(Int => Boolean)):List[Int]= {
    computeNearestNeighbors(node).filter(filter).take(k)
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
  def getKNearest(k:Int,filter:(Int => Boolean)=(_=>true))(node:Int):Iterable[Int] = {
    if(k >= N-1) return Nodes
    if(!closestNeighbors.isDefinedAt(k)){
      saveKNearestPoints(k:Int,filter)
    }
    updateMaxAvgUnrouted(k,node)
    closestNeighbors(k)(node)
  }

  /**
   * Returns the k nearest neighbor of a given point.
   * It allows us to add a filter (optional) on the neighbor.
   * @param k the parameter k.
   * @param node the given point.
   * @param filter the filter.
   * @return the k nearest neighbor as an iterable list of Int.
   */
  def getKNearestNeighbors(k:Int, node:Int,filter:(Int => Boolean) = ( _ => true)):Iterable[Int] = {
    if (k >= N-1) return Nodes
    if(!closestNeighbors.isDefinedAt(k)){
      saveKNearestPoints(k:Int,filter)
    }
    updateMaxAvgUnrouted(k,node)
    closestNeighbors(k)(node)
  }

  /**
    * Update the maximum average of unrouted node in the k nearest neighbor of a given point.
    * @param k the parameter k.
    * @param node the given point.
    */
  def updateMaxAvgUnrouted(k:Int,node:Int){
    var avg : Double = 0
    closestNeighbors(k)(node).foreach(n => if(!isRouted(n)) avg += 1)
    if (avg/k >maxAvgUnrouted) maxAvgUnrouted = avg/k
  }

}

/**
 * Maintains the hop distance in the VRP, based either on a matrix, or on another mechanism.
 * We consider that a hop distance of Int.MaxVal is unreachable.
 */
trait HopDistance extends VRP {
  /**
   * the data structure which maintains the current hop distance of each node to reach his successor.
   * Info : the domain max is (Int.MaxValue / N) to avoid problem with domain. (allow us to use sum invariant without
   * throw over flow exception to save the distance of all vehicle).
   */
  val hopDistance = Array.tabulate(N) {(i:Int) => new IntVar(m, 0, Int.MaxValue / N, 0, "hopDistanceForLeaving" + i)}

  /**
   * maintains the total distance of all vehicle, linked on the actual next hop of each node.
   */
  val overallDistance: IntVar = Sum(hopDistance)

  /**
   * the function which defines the distance between two points of the VRP.
   */
  var distanceFunction:((Int, Int) => Int) = null

  /**
   * This method sets the function distance with a distance matrix.
   * If a more complex function is to be used, set a controlling invariant to the hopDistances yourself.
   * It considers distance from a node to itself as zero.
   * @param DistanceMatrix the distance between each point.
   */
  def installCostMatrix(DistanceMatrix: Array[Array[Int]]) {
    distanceFunction = (i:Int,j:Int) => DistanceMatrix(i)(j)
    for (i <- 0 until N ) hopDistance(i) <== IntVar2IntVarFun(Next(i), j => {if (j!= N) DistanceMatrix(i)(j) else 0})
  }

  /**
   * This method sets the distance to use for the hop between points thanks
   * to a given function.
   * @param fun the function which defines the distance between two points.
   */
  def installCostFunction(fun:(Int, Int) => Int){
    distanceFunction = fun
    for (i <- 0 until N) hopDistance(i) <== IntVar2IntVarFun(Next(i), j => fun(i,j))
  }

  /**
   * Returns the distance from a given node (start node) to another given node (end node) of the VRP.
   * @param from the start node
   * @param to the end node
   * @return the distance between the start and end node as an Int.
   */
  def getHop(from:Int, to:Int):Int = distanceFunction(from,to)
}


/**
 * Declares an objective function, attached to the VRP.
 * It maintains it equal to the hop distance in the VRP,
 * based either on a matrix, or on another mechanism defined by the distance function.
*/
trait HopDistanceAsObjective extends HopDistance with ObjectiveFunction {
  ObjectiveVar <== overallDistance
}

/**
 * Declares an objective function, attached to the VRP.
*/
trait ObjectiveFunction extends VRP with ObjectiveTrait{
  // Initialize the objective function with 0 as value.
  setObjectiveVar(new IntVar(m, Int.MinValue, Int.MaxValue, 0, "objective of VRP"))
}

/**
 * Allows to add news functions cost to the actual objective of the VRP.
*/
trait OtherFunctionToObjective extends ObjectiveFunction {

  /**
   * variable which maintains the sum of all additional cost functions.
   */
  var AddedObjectiveFunctions:IntVar = new IntVar(m,Int.MinValue,Int.MaxValue,0,"added functions Objective")
  /**
   * Adds news cost functions to the the actual objective of the VRP.
   * Functions cost are given as IntVar.
   * @param functions the additional cost functions.
   */
  def recordAddedFunctions(functions: Iterable[IntVar]){
    assert(!functions.isEmpty && ObjectiveVar!= null)

    var objAdd = AddedObjectiveFunctions
    functions.foreach(f => { objAdd = objAdd + f })
    AddedObjectiveFunctions = AddedObjectiveFunctions + objAdd
    objAdd = ObjectiveVar + objAdd
    setObjectiveVar(objAdd)
   }

  /**
   * Add a new cost function to the actual objectif of the VRP.
   * Function cost is given as IntVar.
   * @param function the additional cost function.
   */
  def recordAddedFunction(function : IntVar) = recordAddedFunctions(Array[IntVar](function))
}

/**
 * Maintains the position of nodes in the routes, the route number of each node,
 * the length of each route and their last node.
*/
trait PositionInRouteAndRouteNr extends VRP {
  /**
   * the invariant Routes.
   */
  val routes = Routes.buildRoutes(Next, V)
  /**
   * the position in route of each node as an array of IntVar.
   */
  val PositionInRoute = routes.PositionInRoute
  /**
   * the route number of each node as an array of IntVar.
   */
  val RouteNr = routes.RouteNr
  /**
   * the route length of each route as an array of IntVar.
   */
  val RouteLength = routes.RouteLength

  /**
   * Tells if twos given nodes form a segment of route of n minimum length.
   * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @param n the minimum length of segment.
   * @return true if "fromNode" to "toNode" forms a segment of route of n minimum length, else false.
   */
  def isAtLeastAsFarAs(fromNode:Int, toNode:Int, n:Int):Boolean = {
    RouteNr(fromNode).value == RouteNr(toNode).value &&
      PositionInRoute(fromNode).value + n  <= PositionInRoute(toNode).value
  }

  /**
   * Tells if two given nodes form a segment of route of n maximum length.
   * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @param n the maximum length of route.
   * @return true if "fromNode" to "toNode" forms a segment of route of n maximum length, else false.
   */
  def isAtMostAsFarAs(fromNode:Int,toNode:Int,n:Int):Boolean = {
    RouteNr(fromNode).value == RouteNr(toNode).value &&
      PositionInRoute(fromNode).value + n  >= PositionInRoute(toNode).value
  }

  /**
   * Tells if two given nodes form a segment of route.
   * @param fromNode the start of potential segment.
   * @param toNode the end of potential segment.
   * @return true if "fromNode" to "toNode" form a segment of route, else false.
   */
  def isASegment(fromNode:Int,toNode:Int):Boolean = {
    isAtLeastAsFarAs(fromNode,toNode,1)
  }

  /**
   * Tells if a given node is in a segment of route between fromNode and toNode.
   * @param node the given node queried.
   * @param fromNode the start of the segment of route.
   * @param toNode the end of the segment of route.
   * @return true if node is in a segment of route between "fromNode" and "toNode", else false.
   */
  def isBetween(node:Int,fromNode:Int,toNode:Int):Boolean = {
    if(isASegment(fromNode,toNode)){
      return RouteNr(fromNode).value == RouteNr(node).value  &&
        PositionInRoute(fromNode).value <= PositionInRoute(node).value &&
          PositionInRoute(node).value < PositionInRoute(toNode).value
    }
    else false
  }

  /**
   * Tells if two given nodes are on the same route.
   * ( i.e. they have the same route number)
   * @param n the first given node.
   * @param m the second given node.
   */
  def onTheSameRoute(n:Int,m:Int):Boolean = {
    RouteNr(n).value == RouteNr(m).value
  }
}

/**
 * This trait maintains the predecessors of each node of the VRP.
 * It uses the Predecessor invariant.
 */
trait Predecessors extends VRP{
  /**
   * the data structure array which maintains the predecessors of each node.
   */
  val preds = Predecessor(Next,V)
}

/**
 * This trait maintains strong constraints system.
 * It redefines the propagation method of ObjectiveFunction trait,
 * that saves time by propagating partially.
 */
trait StrongConstraints extends ObjectiveFunction {
  /**
   * the strong constraints system.
   */
  var strongConstraints:ConstraintSystem = null
  var violatedStrongConstraints = false

  /**
   * This method attachs a strong constraints system to the VRP.
   * @param sc the strong constraints system we want to attach to the VRP.
   */
  def setStrongConstraints(sc:ConstraintSystem) {strongConstraints = sc}

  /**
   * Update the violation of the strong constraints system.
   */
  def updateViolatedStrongConstraints {
    if(strongConstraints == null) violatedStrongConstraints  = false
    else
      violatedStrongConstraints = !strongConstraints.isTrue
  }

  override def propagateObjective:Int = {
    updateViolatedStrongConstraints
    if (violatedStrongConstraints) Int.MaxValue else ObjectiveVar.value
  }
}

/**
 * This trait maintains weak constraints system.
 */
trait WeakConstraints extends OtherFunctionToObjective {
  /**
   * the weak constraints system.
   */
  var weakConstraints:ConstraintSystem = null

  /**
   * This method attachs a weak constraints system to the VRP.
   * The penalty's weight of this system is automatically added to the objective of the VRP.
   * @param wc the weak constraints system we want to attach to the VRP.
   */
  def setWeakConstraints(wc:ConstraintSystem) {weakConstraints = wc;recordAddedFunction(weakConstraints.violation)}
}


/**
 * This trait helps evaluate faster some operators of neighborhood as the two-opt and the three-opt,
 * in case of symmetric instance of VRP.
 *
 * Info : to use with precaution, its evaluations doesn't take into account of the added objective functions.
 * Evaluation is based only on hop distance function.
 */
trait SymmetricVRP extends HopDistance{

  /**
   * Returns if it's effective to do a tree-opt (withtout reverse) move, based only on hop distance function.
   * @param a start of first edge of a three-opt move.
   * @param b end of first edge of a three-opt move.
   * @param c start of second edge of a three-opt move.
   * @param d end of second edge of a three-opt move.
   * @param e start of third edge of a three-opt move.
   * @param f end of third edge of a three-opt move.
   * @return true if it's interesting to do a three-opt move, evaluated on hop distance function, else false.
   */
  def isEffectiveThreeOptA(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):Boolean = isEffectiveThreeOptB(a,b,c,d,e,f)

  /**
   * Returns if it's effective to do a tree-opt (with one reverse) move, based only on hop distance function.
   * @param a start of first edge of a three-opt move.
   * @param b end of first edge of a three-opt move.
   * @param c start of second edge of a three-opt move.
   * @param d end of second edge of a three-opt move.
   * @param e start of third edge of a three-opt move.
   * @param f end of third edge of a three-opt move.
   * @return true if it's interesting to do a three-opt move, evaluated on hop distance function, else false.
   */
  def isEffectiveThreeOptB(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):Boolean = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    val delta = - (hopDistance(a).value + hopDistance(c).value + hopDistance(e).value)
    (distanceFunction(a,e) + distanceFunction(d,b) + distanceFunction(c,f) + delta)<0
  }
  /**
   * Returns if it's effective to do a tree-opt (with two reverses) move, based only on hop distance function.
   * @param a start of first edge of a three-opt move.
   * @param b end of first edge of a three-opt move.
   * @param c start of second edge of a three-opt move.
   * @param d end of second edge of a three-opt move.
   * @param e start of third edge of a three-opt move.
   * @param f end of third edge of a three-opt move.
   * @return true if it's interesting to do a three-opt move, evaluated on hop distance function, else false.
   */
  def isEffectiveThreeOptC(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):Boolean = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    val delta = - (hopDistance(a).value + hopDistance(c).value + hopDistance(e).value)
    (distanceFunction(a,c) + distanceFunction(b,e) + distanceFunction(d,f) + delta)<0
   }

  /**
   * Returns if it's effective to do a two-opt move, based only on hop distance function.
   * @param a start of first edge of a two-opt move.
   * @param b end of first edge of a two-opt move.
   * @param c start of second edge of a two-opt move.
   * @param d end of second edge of a two-opt move.
   * @return true if it's interesting to do a two-opt move, evaluated on hop distance function, else false.
   */
  def isEffectiveTwoOpt(a:Int,b:Int,c:Int,d:Int):Boolean = {
    val delta = - (hopDistance(a).value + hopDistance(c).value)
    (distanceFunction(a,c) + distanceFunction(b,d) + delta)<0
  }

  /**
   * Returns the list of variables to update with theirs new values in order to perform
   * a two-opt move, which is smart evaluated.
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  override def twoOpt(a:Int,b:Int,c:Int,d:Int):List[(IntVar,Int)]= {
    if(isEffectiveTwoOpt(a,b,c,d)) super.twoOpt(a,b,c,d)
    else List.empty
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * perform a three-opt (without reverse) move, which is smart evaluated.
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @param e the start of edge (e,f).
   * @param f the end of edge (e,f).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  override def threeOptA(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    if (isEffectiveThreeOptA(a,b,c,d,e,f))
      super.threeOptA(a,b,c,d,e,f)
    else List.empty
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * perform a three-opt (with one reverse) move, which is smart evaluated.
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @param e the start of edge (e,f).
   * @param f the end of edge (e,f).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  override def threeOptB(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    if (isEffectiveThreeOptB(a,b,c,d,e,f))
      super.threeOptB(a,b,c,d,e,f)
    else List.empty
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * perform a three-opt (with two reverses) move, which is smart evaluated.
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @param e the start of edge (e,f).
   * @param f the end of edge (e,f).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  override def threeOptC(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    if (isEffectiveThreeOptC(a,b,c,d,e,f))
      super.threeOptC(a,b,c,d,e,f)
    else List.empty
  }
}

