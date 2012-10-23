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
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.logic.Predecessor
import oscar.cbls.invariants.lib.set.TakeAny
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

  def reverseSegmentListToUpdate(from:Int, to:Int):List[(IntVar,Int)]={
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
   * @param insertionPoint the point after which insert the segment.
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
   def moveSegmentListToUpdate(beforeSegmentStart:Int, segmentEnd:Int,  insertionPoint:Int):List[(IntVar,Int)] = {
    val segmentStart:Int = Next(beforeSegmentStart).getValue(true)
    val oldNextOfSegmentEnd:Int = Next(segmentEnd).getValue(true)
    val oldNextOfInsertionPoint:Int = Next(insertionPoint).getValue(true)

    (Next(beforeSegmentStart),oldNextOfSegmentEnd)::(Next(segmentEnd),oldNextOfInsertionPoint)::
      (Next(insertionPoint),segmentStart)::List.empty
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
  def unrouteListToUpdate(l:Iterable[(Int,Int)]):List[(IntVar,Int)]={

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
  def flipListToUpdate(a:Int,b:Int,c:Int,d:Int):List[(IntVar,Int)] = {
    assert(Next(a).value==b && Next(c).value==d)
    assert(c != b) // else useless to flip
    (Next(a),c)::(Next(b),d)::reverseSegmentListToUpdate(b,c)
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

  def flipWith2ReverseListToUpdate(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    //assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    var listToUpdate:List[(IntVar,Int)] = List.empty
    listToUpdate = (Next(a),c)::listToUpdate
    listToUpdate = reverseSegmentListToUpdate(b,c):::listToUpdate
    listToUpdate = (Next(b),e)::listToUpdate
    listToUpdate = reverseSegmentListToUpdate(d,e):::listToUpdate
    listToUpdate = (Next(d),f)::listToUpdate
    listToUpdate
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
  def flipWith1ReverseListToUpdate(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    var listToUpdate:List[(IntVar,Int)] = List.empty
    listToUpdate = (Next(a),e)::listToUpdate
    listToUpdate = reverseSegmentListToUpdate(d,e):::listToUpdate
    listToUpdate = (Next(d),b)::listToUpdate
    listToUpdate = (Next(c),f)::listToUpdate
    listToUpdate
  }

}

/**Maintains the set of unrouted nodes, those whose next is zero*/
trait Unrouted extends VRP {
  //val Unrouted: IntSetVar = Filter(Next, (next: Int) => next == 0)
  val Unrouted: IntSetVar = Filter(Next, (next: Int) => next == N)
}

/**add this trait if you want to maintain the predecessor of each node in the routing.
 * It also maintains the set of unrouted nodes.
 * This other feature is integrated with the mechanics for maintaining the predecessors, so it comes at cost zero.
 */
//TODO regarder à la structure de donné tableau.

trait PredAndUnrouted extends Unrouted {


  /*
  private val Preds: Array[IntSetVar] = Cluster.MakeDense(Next).clusters
  val Pred: Array[IntVar] = Preds.map((i: IntSetVar) => TakeAny(i, 0).toIntVar)
  override val Unrouted: IntSetVar = Preds(N)
  */
  // new invariant Predecessor
  val preds = Predecessor(Next)
  // use SparseCluster for unrouted node
  override val Unrouted: IntSetVar = Cluster.MakeSparse(Next,List(N)).Clusters(N)
  // or we can use Filter invariant.
}




/**maintains the position of nodes in the routes and the route number of each node*/
trait PositionInRouteAndRouteNr extends VRP {
  /*private*/ val routes = Routes.buildRoutes(Next, V)

  val PositionInRoute = routes.PositionInRoute
  val RouteNr = routes.RouteNr

  def isAtLeastAsFarAs(fromNode:Int, toNode:Int, n:Int):Boolean = {
    RouteNr(fromNode).value == RouteNr(toNode).value &&
      PositionInRoute(fromNode).value + n  <= PositionInRoute(toNode).value
  }

  def isASegment(fromNode:Int,toNode:Int):Boolean = {
    isAtLeastAsFarAs(fromNode,toNode,1)
  }


  /**assuming fromNode,toNOde form a segment*/
  def isBetween(node:Int,fromNode:Int,toNode:Int):Boolean = {
    RouteNr(fromNode).value == RouteNr(node).value  &&
    PositionInRoute(fromNode).value <= PositionInRoute(node).value &&
    PositionInRoute(node).value < PositionInRoute(toNode).value
  }



}

trait OptimizeThreeOptWithReverse extends VRP with HopDistanceAsObjective{

  def getEffectivenessFlipWith1ReverseListToUpdate(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):Int = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    val delta = - (hopDistance(a).value + hopDistance(c).value + hopDistance(e).value)
    distanceFunction(a,e) + distanceFunction(d,b) + distanceFunction(c,f) + delta
  }

  def getEffectivenessFlipWith2ReverseListToUpdate(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):Int = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    val delta = - (hopDistance(a).value + hopDistance(c).value + hopDistance(e).value)
    distanceFunction(a,c) + distanceFunction(b,e) + distanceFunction(d,f) + delta
   }
}



/**declares an objective function, attached to the VRP. */
trait ObjectiveFunction extends VRP with ObjectiveTrait{
  // Initialize the objective function with 0 as value
  setObjectiveVar(new IntVar(m, 0, Int.MaxValue, 0, "objective of VRP"))
}

/**maintains the hop distance in the VRP, based either on a matrix, or on another mechanism. *
 * We consider that a hop distance of Int.MaxVal is unreachable
 */
trait HopDistance extends VRP {
  // 40 000 is hard code for the circumference of the earth.
  val hopDistance = Array.tabulate(N) {(i:Int) => new IntVar(m, 0, 40000, 0, "hopDistanceForLeaving" + i)}

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
    ObjectiveVar<== overallDistance + AddedValue
  }
}

/**finds the nearest neighbor of each point
 * used by some neighborhood searches
 */
trait ClosestNeighborPoints extends VRP with HopDistance{
  var closestNeighbors:SortedMap[Int, Array[List[Int]]] = SortedMap.empty

  def saveKNearestPoints(k:Int){
    val neighbors = Array.tabulate(N)((node:Int) => computeKNearestNeighbors(node, k))
    closestNeighbors += ((k,neighbors))
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
