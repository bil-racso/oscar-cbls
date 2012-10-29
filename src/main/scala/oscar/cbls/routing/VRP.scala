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

import oscar.cbls.invariants.core.computation.{Invariant, IntSetVar, IntVar, Model}
import oscar.cbls.invariants.lib.numeric.{SumElements, ProdElements, Sum}
import oscar.cbls.invariants.lib.logic.Predecessor
import oscar.cbls.invariants.lib.set.{Cardinality, TakeAny}
import oscar.cbls.invariants.lib.logic.{Filter, IntVar2IntVarFun, Routes, Cluster}
import oscar.cbls.algebra.Algebra._
import oscar.cbls.constraints.core.Constraint
import collection.immutable.SortedMap
import scala.math._
import oscar.cbls.objective.{ObjectiveTrait, Objective}

/**
 * @param N the number of points in the problem.
 * @param V the number of vehicles, assumed to start from points 0 to V-1.
 * @param m the model.
 */
class VRP(val N: Int, val V: Int, val m: Model) {
  // nb of reverse segment
  var reverseNb = 0 //TODO to del, only there as information

  //successors
  val Next: Array[IntVar] = Array.tabulate(N)(i => if(i<V) new IntVar(m, i, N-1, i, "next" + i)
    else new IntVar(m, 0, N, i, "next" + i))
  for(v <- 0 until V){Next(v) := v}
  val Nodes = 0 until N

  /**
   * Redefine the toString method.
   * @return The VRP as a String
   */
  override def toString():String = {
    var toReturn = ""
    for ( v <- 0 until V){
      toReturn += "Vehicle" + v + ":"
      var current = Next(v).value
      while(current != v){
        toReturn += " " + current
        current = Next(current).getValue(true)
      }
      toReturn+="\n"
    }
    toReturn
  }


  /**
   * Returns the list of variables to update with theirs new values in order to reverse
   * a segment of route.
   *
   * @param from the start of segment to reverse.
   * @param to the end of segment to reverse.
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def reverse(from:Int, to:Int):List[(IntVar,Int)]={
    var listToUpdate:List[(IntVar,Int)] = List.empty
    var nodeStack:List[Int] = List.empty
    //register the list of nodes
    var current:Int = from
    while(current != to){
      nodeStack = current :: nodeStack
      current = Next(current).value
    }
    while(!nodeStack.isEmpty){
      reverseNb += 1 //TODO to del, only there as information
      listToUpdate =(Next(current),nodeStack.head)::listToUpdate
      current = nodeStack.head
      nodeStack = nodeStack.tail
    }
    listToUpdate
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * move a segment and reinsert it after a given point.
   * @param beforeSegmentStart the predecessor of segment's start.
   * @param segmentEnd the segment's end.
   * @param beforeSecondSwapPoint the point after which insert the segment.
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
   def moveTo(beforeSegmentStart:Int, segmentEnd:Int,  beforeSecondSwapPoint:Int):List[(IntVar,Int)] = {
    val segmentStart:Int = Next(beforeSegmentStart).getValue(true)
    val oldNextOfSegmentEnd:Int = Next(segmentEnd).getValue(true)
    val oldNextOfbeforeSecondSwapPoint:Int = Next(beforeSecondSwapPoint).getValue(true)

    (Next(beforeSegmentStart),oldNextOfSegmentEnd)::(Next(segmentEnd),oldNextOfbeforeSecondSwapPoint)::
      (Next(beforeSecondSwapPoint),segmentStart)::List.empty
  }

  /** Returns the list of variables to update with theirs new values in order to
    * remove points or segments of a route. The points or segments to remove
    * are given in an iterable list of tuple (first,second) of Integer. The first Integer
    * is the predecessor of the first point to remove, and the second is the last
    * point to remove. This assumes that (first,second) is a segment, and
    * the segments formed by the tuples of the list must be disjoint.
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

  /*
  Adds an unrouted point in a route at a given insertion point.
  Assumes that point is an unrouted one.
   */
  def add(insertion:Int,point:Int):Iterable[(IntVar,Int)] = {
    val next = Next(insertion).value
    List((Next(insertion),point),(Next(point),next))
  }




  /**
   * Returns the list of variables to update with theirs new values in order to
   * update routes by swapping two nodes, a and b.
   *
   * @param before_a the node before a node.
   * @param a the node which one swap with b node.
   * @param before_b the node before b node.
   * @param b the node which one swap with a node.
   */
   def swap(before_a:Int,a:Int,before_b:Int,b:Int):List[(IntVar,Int)] = {
    val next_a:Int = Next(a).value
    val next_b:Int = Next(b).value
    (Next(before_a),b)::(Next(b),next_a)::(Next(before_b),a)::(Next(a),next_b)::List.empty
   }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * update a route by replacing the edges (a,b) and (c,d) by the
   * edges (a,c) and (b,d), and reverse the route's segment [b;c]. This assumes that b = Next(a) and d = Next(c).
   * (This is a 2-OPT move.)
   *
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
   * Override the two-opt move.
   * @param a the start of edge (a,b).
   * @param b the end of edge (a,b).
   * @param c the start of edge (c,d).
   * @param d the end of edge (c,d).
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def twoOpt(a:Int,b:Int,c:Int,d:Int):List[(IntVar,Int)] = flip(a,b,c,d)

  /**
   * Override the three-opt move without reverse.
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
 * edges (a,e), (d,b),(c,f), and reverse the route's segment [d;e].
 * This assumes that b = Next(a), d = Next(c), f = Next(e) and the order of segment's route
 * is fixed as (a,b) appears first, (c,d) appears second, and finally meets the segment (e,f) in the tour.
 * (This is a 3-OPT move)
 *
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
    listToUpdate = (Next(a),e)::listToUpdate
    listToUpdate = reverse(d,e):::listToUpdate
    listToUpdate = (Next(d),b)::listToUpdate
    listToUpdate = (Next(c),f)::listToUpdate
    listToUpdate
    // version using flip (make cycle in propagation cause of overlapping reverse)
    // must commit the first flip to avoid problem.
    //flip(a,b,e,f)::flip(d,c,b,f)
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * update a route by replacing the edges (a,b), (c,d) and (e,f) by the
   * edges (a,c), (b,e),(d,f), and reverse the route's segment [b;c] and [e;d]
   * This assumes that b = Next(a), d = Next(c), f = Next(e) and the order of segment's route
   * is fixed as (a,b) appears first, (c,d) appears second, and finally meets the segment (e,f) in the tour.
   * (This is a 3-OPT move)
   *
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
    /*
      var listToUpdate:List[(IntVar,Int)] = List.empty
      listToUpdate = (Next(a),c)::listToUpdate
      listToUpdate = reverse(b,c):::listToUpdate
      listToUpdate = (Next(b),e)::listToUpdate
      listToUpdate = reverse(d,e):::listToUpdate
      listToUpdate = (Next(d),f)::listToUpdate
      listToUpdate
    */
    // using flip
    flip(a,b,c,d):::flip(b,d,e,f)
  }
}

/**
 * Maintains a penalty weight for unrouted nodes.
 */
trait PenaltyForUnrouted extends Unrouted {
  val weightPenalty : Array[IntVar] = Array.tabulate(N)(i => new IntVar(m, Int.MinValue, Int.MaxValue, 0,
    "penality of node " + i))
  val Penalty : IntVar = SumElements(weightPenalty,Unrouted)

  def fixPenaltyWeight(i:Int,w:Int) { weightPenalty(i) := w}
  def fixPenaltyWeight(w:Int) {weightPenalty.foreach(p => p := w)}
}

/**
 * Maintains the set of unrouted nodes, those whose next is N.
*/
trait Unrouted extends VRP {
  val Unrouted: IntSetVar = Filter(Next, (next: Int) => next == N)
}

/**
 * Add this trait if you want to maintain the predecessors of each node in the routing.
 */
trait Predecessors extends VRP{
  val preds = Predecessor(Next)
}


/**
 * Maintains the position of nodes in the routes, the route number of each node,
 * the length of each route and their last node.
*/
trait PositionInRouteAndRouteNr extends VRP {
  val routes = Routes.buildRoutes(Next, V)
  val PositionInRoute = routes.PositionInRoute
  val RouteNr = routes.RouteNr
  val RouteLength = routes.RouteLength

  /**
   * Tells if fromNode to toNode forms a segment of route of n minimum length.
   * @param fromNode the start of route.
   * @param toNode the end of route.
   * @param n the minimum length of route.
   * @return if fromNode to toNode forms a segment of route of n minimum length.
   */
  def isAtLeastAsFarAs(fromNode:Int, toNode:Int, n:Int):Boolean = {
    RouteNr(fromNode).value == RouteNr(toNode).value &&
      PositionInRoute(fromNode).value + n  <= PositionInRoute(toNode).value
  }

  /**
   * Tells if fromNode to toNode form a segment of route.
   * @param fromNode the start of segment.
   * @param toNode the end of segment.
   * @return Tells if fromNode to toNode form a segment of route.
   */
  def isASegment(fromNode:Int,toNode:Int):Boolean = {
    isAtLeastAsFarAs(fromNode,toNode,1)
  }

  /**
   * Tells if node is in a segment of route between fromNode and toNode.
   * @param node a node.
   * @param fromNode the start of the segment of route.
   * @param toNode the end of the segment of route.
   * @return if node is in a segment of route between fromNode and toNode.
   */
  def isBetween(node:Int,fromNode:Int,toNode:Int):Boolean = {
    assert(isASegment(fromNode,toNode))
    RouteNr(fromNode).value == RouteNr(node).value  &&
    PositionInRoute(fromNode).value <= PositionInRoute(node).value &&
    PositionInRoute(node).value < PositionInRoute(toNode).value
  }
}

/**
 * In case of symmetric instance of VRP, it helps evaluate faster some operators
 * of neighborhood as the two-opt and the three-opt.
 */
trait SymmetricVRP extends HopDistanceAsObjective{

  def isEffectiveThreeOptB(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):Boolean = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    val delta = - (hopDistance(a).value + hopDistance(c).value + hopDistance(e).value)
    (distanceFunction(a,e) + distanceFunction(d,b) + distanceFunction(c,f) + delta)<0
  }

  def isEffectiveThreeOptC(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):Boolean = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    val delta = - (hopDistance(a).value + hopDistance(c).value + hopDistance(e).value)
    (distanceFunction(a,c) + distanceFunction(b,e) + distanceFunction(d,f) + delta)<0
   }

  def isEffectiveTwoOpt(a:Int,b:Int,c:Int,d:Int):Boolean = {
    val delta = - (hopDistance(a).value + hopDistance(c).value)
    (distanceFunction(a,c) + distanceFunction(b,d) + delta)<0
  }

  override def twoOpt(a:Int,b:Int,c:Int,d:Int):List[(IntVar,Int)]= {
    if(isEffectiveTwoOpt(a,b,c,d)) super.twoOpt(a,b,c,d)
    else List.empty
  }

  override def threeOptC(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    if (isEffectiveThreeOptC(a,b,c,d,e,f))
      super.threeOptC(a,b,c,d,e,f)
    else List.empty
  }
  override def threeOptB(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    if (isEffectiveThreeOptB(a,b,c,d,e,f))
      super.threeOptB(a,b,c,d,e,f)
    else List.empty
  }
}




/**
 * Declares an objective function, attached to the VRP.
*/
trait ObjectiveFunction extends VRP with ObjectiveTrait{
  // Initialize the objective function with 0 as value
  // allow negative objective value
  setObjectiveVar(new IntVar(m, Int.MinValue, Int.MaxValue, 0, "objective of VRP"))
}

/**
 * Maintains the hop distance in the VRP, based either on a matrix, or on another mechanism.
 * We consider that a hop distance of Int.MaxVal is unreachable
 */
trait HopDistance extends VRP {
  // Int.MaxValue / N allow us to use sum invariant, else MaxValue of this invariant is over Integer.MaxValue.
  val hopDistance = Array.tabulate(N) {(i:Int) => new IntVar(m, 0, Int.MaxValue / N, 0, "hopDistanceForLeaving" + i)}

  val overallDistance: IntVar = Sum(hopDistance)

  /**This method sets the distance to use for the hop between points.
   * If a more complex function is to be used, set a controlling invariant to the hopDistances yourself
   * @param DistanceMatrix is the distance between each point. All distance involving point 0 must be zero (they are corrected anyway by the engine)
   */
  def installCostMatrix(DistanceMatrix: Array[Array[Int]]) {
    distanceFunction = (i:Int,j:Int) => DistanceMatrix(i)(j)
    for (i <- 0 until N ) hopDistance(i) <== IntVar2IntVarFun(Next(i), j => {if (j!= N) DistanceMatrix(i)(j) else 0})
  }

  def installCostFunction(fun:(Int, Int) => Int){
    distanceFunction = fun
    for (i <- 0 until N) hopDistance(i) <== IntVar2IntVarFun(Next(i), j => fun(i,j))
  }
  
  var distanceFunction:((Int, Int) => Int) = null
  def getHop(from:Int, to:Int):Int = distanceFunction(from,to)
}

/**declares an objective function, attached to the VRP.
 * And maintains it equal to the hop distance in the VRP,
 * based either on a matrix, or on another mechanism.
*/
trait HopDistanceAsObjective extends HopDistance with ObjectiveFunction {
  ObjectiveVar <== overallDistance
}

/**declares an objective function, attached to the VRP.
 * And maintains it equal to an AddedValue plus the hop distance in the VRP,
 * based either on a matrix, or on another mechanism.
*/
trait HopDistanceAndOtherAsObjective extends HopDistance with ObjectiveFunction {
  def recordAddedFunction(AddedValue: IntVar) {
    ObjectiveVar <== overallDistance + AddedValue
  }
}

/**finds the nearest neighbor of each point
 * used by some neighborhood searches
 */
trait ClosestNeighborPoints extends VRP with HopDistance{
  var closestNeighbors:SortedMap[Int, Array[List[Int]]] = SortedMap.empty

  def saveKNearestPoints(k:Int){
    if (k < N-1){
      val neighbors = Array.tabulate(N)((node:Int) => computeKNearestNeighbors(node, k))
      closestNeighbors += ((k,neighbors))
    }
  }
  
  def computeKNearestNeighbors(node:Int, k:Int):List[Int] = {
    val reachableneigbors = Nodes.filter((next:Int)
      => node != next && (getHop(node,next)!= Int.MaxValue || getHop(next, node)!= Int.MaxValue))
    //TODO: this is deeply inefficient. use a lazy quicksort instead, orr a partial sort based on a heap?
    val sortedneighbors = reachableneigbors.sortBy((neigbor:Int) => min(getHop(neigbor, node),getHop(node,neigbor)))
    sortedneighbors.toList.take(k)
  }

  def getKNearestNeighbors(k:Int, node:Int):Iterable[Int] = {
    if (k >= N-1) return Nodes
    if(!closestNeighbors.isDefinedAt(k)){
      saveKNearestPoints(k:Int)
    }
    closestNeighbors(k)(node)
  }
}
