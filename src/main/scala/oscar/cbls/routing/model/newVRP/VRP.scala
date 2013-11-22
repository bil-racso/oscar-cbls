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
/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by De Landtsheer Renaud and Ghilain Florent.
  ******************************************************************************/

package oscar.cbls.routing.model.newVRP


import collection.immutable.SortedMap
import math._
import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.invariants.core.computation.{IntSetVar, Model, IntVar}
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.core.algo.heap.BinomialHeap
import oscar.cbls.invariants.lib.numeric.SumElements
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.logic.Filter
import oscar.cbls.invariants.lib.logic.Predecessor

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
 */
class VRP(val N: Int, val V: Int, val m: Model) {
  /**
   * the data structure array which maintains the successors.
   * It assumed that the V vehicles are indexed from the point 0 to V-1,
   * like that each vehicle is considered like a deposit. Other indexes
   * are used to modelise customers. Finally the value N is used for unrouted node.
   */
  val Next: Array[IntVar] = Array.tabulate(N)(i => if(i<V) IntVar(m, V to  N-1, i, "next" + i)
  else IntVar(m, 0, N, N, "next" + i))

  /**unroutes all points of the VRP*/
  def unroute(){
    for (i <- 0 until V) Next(i) := i
    for (i <- V until N) Next(i) := N
  }

  /**unroutes points of vehicle v*/
  def unroute(v:Int){
    doRemove(List((v,v)))
  }

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

  /** performs the described move
    * @param move a set of ssignments to perform on the road
    */
  def doIt(move:List[(IntVar,Int)]){
    for((v:IntVar,i:Int) <- move) v := i
  }

  //TODO: tous les mouvements peuvent être réécrits en intégrant la notion de segment (from,to), et de segment (beforeFrom,BeforeTo)
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
   * reverses a segment of route.
   * @param from the start of segment to reverse.
   * @param to the end of segment to reverse.
   */
  def doReverse(from:Int, to:Int){
    doIt(reverse(from, to))
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

  /**
   * moves a segment and reinsert it after a given point.
   * @param beforeSegmentStart the predecessor of the start of segment.
   * @param segmentEnd the end of segment.
   * @param insertionPoint the point after which to insert the segment.
   */
  def doMoveTo(beforeSegmentStart:Int, segmentEnd:Int,  insertionPoint:Int){
    doIt(moveTo(beforeSegmentStart:Int, segmentEnd:Int,  insertionPoint:Int))
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
  def doRemove(l:Iterable[(Int,Int)]){
    doIt(remove(l:Iterable[(Int,Int)]))
  }

  /**
   * Returns the list of variables to update with theirs new values in order to
   * add an unrouted point in a route at a given insertion point.
   * Assumes that the point is an unrouted one.
   * @param insertion the place where insert the unrouted node.
   * @param point the unrouted node.
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def add(insertion:Int,point:Int):List[(IntVar,Int)] = {
    assert(!isRouted(point))
    val next = Next(insertion).value
    List((Next(insertion),point),(Next(point),next))
  }

  /**
   * add an unrouted point in a route at a given insertion point.
   * Assumes that the point is an unrouted one.
   * @param insertion the place where insert the unrouted node.
   * @param point the unrouted node.
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def doAdd(insertion:Int,point:Int){
    doIt(add(insertion:Int,point:Int))
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
   * update routes by swapping two nodes, a and b.
   * @param before_a the node before node "a".
   * @param a the node which one swap with node "b".
   * @param before_b the node before node "b".
   * @param b the node which one swap with node "a".
   * @return list of tuple (IntVar,Int) where IntVar is a variable to update and Int is his new value.
   */
  def doSwap(before_a:Int,a:Int,before_b:Int,b:Int){
    doIt(swap(before_a:Int,a:Int,before_b:Int,b:Int))
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
  def doFlip(a:Int,b:Int,c:Int,d:Int){
    doIt(flip(a:Int,b:Int,c:Int,d:Int))
  }

  /**
   * Redefine the toString method.
   * @return the VRP problem as a String.
   */
  override def toString:String = {
    var toReturn = ""
    for ( v <- 0 until V){
      toReturn += "Vehicle" + v + ":" + v
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



trait VRPObjective extends VRP{

  private val landmarkArray: Array[Int] = Array.tabulate(N)( _ => -1)
  private var landmarkedList:List[Int] = List.empty
  private var landMarking:Boolean = false
  private val NodePositionAccessorKey:Int = m.getStorageIndex

  for(i <- 0 to N-1){
    Next(i).storeAt(NodePositionAccessorKey,new Integer(i))
  }

  private var oldObjective:Int = 0
  val objectiveFunction = IntVar(m, Int.MinValue, Int.MaxValue, 0, "objective of VRP")
  m.registerForPartialPropagation(objectiveFunction)

  private var objectiveFunctionTerms:List[IntVar] = List.empty

  /** adds a term top the objective function*/
  def addObjectiveTerm(o:IntVar){
    objectiveFunctionTerms = o :: objectiveFunctionTerms
  }

  m.addToCallBeforeClose(_=>closeObjectiveFunction)

  /** This finished the accumulation of terms in the objective unction.
    * You should not call this, actually.
    * it is called by the model on close
    */
  def closeObjectiveFunction(){
    objectiveFunction <= Sum(objectiveFunctionTerms)
  }

  /**
   * Sets current point as comparison point for the improves method
   * @param withRestore set to true if you want the routes to be saved, so that you can restore them by calling restoreLandmark
   */
  def defineLandmark(withRestore:Boolean = false){
    cleanLandMark
    landMarking = withRestore
    oldObjective = objectiveFunction.value
  }

  def cleanLandMark{
    if (!landMarking) return
    for(i <- landmarkedList){
      landmarkArray(i) = -1
    }
    landmarkedList = List.empty
  }

  def forgetLandMark(){
    cleanLandMark
    landMarking = false
  }

  /**
   * compares the current solution with the landmark
   * returns the delta on the objectiveFunction
   * @return
   */
  def deltaFromLandMark:Int = {
    objectiveFunction.value - oldObjective
  }

  /** restores the state of the route as they ware when defineLandmark(true) was called for hte last time
    * does not stop the landmarking
    */
  def restoreLandmark{
    if (!landMarking) throw new Error("you cannot restore a landmark if you did not specify defineLandmark(true)")
    for(i <- landmarkedList){
      Next(i) := landmarkArray(i)
      landmarkArray(i) = -1
    }
    landmarkedList = List.empty
  }

  /** performs the described move
    * @param move a set of ssignments to perform on the road
    */
  override def doIt(move:List[(IntVar,Int)]){
    for((v:IntVar,i:Int) <- move){
      if(landMarking){
        val position:Int = Next(i).storeAt(NodePositionAccessorKey,new Integer(i)).asInstanceOf[Integer]
        if(landmarkArray(position) == -1){
          //not landMarked yet
          landmarkArray(position) = v.value
          landmarkedList = position :: landmarkedList
        }
      }
      v := i
    }
  }
}


/**
 * Maintains the set of unrouted nodes.
 * Info : those whose next is N.
 * This trait is abstract, sinbce unrouted can be implemented either stand alone, or as a side effect of other traits
 */
abstract trait Unrouted{
  def unrouted:IntSetVar
}

/**
 * Maintains the set of unrouted nodes.
 * Info : those whose next is N.
 */
trait UnroutedImpl extends VRP with Unrouted{
  /**
   * the data structure set which maintains the unrouted node.
   */
  final override val unrouted: IntSetVar = Filter(Next, (next: Int) => next == N)
  m.registerForPartialPropagation(unrouted)
}

/**
 * Maintains and fixes a penalty weight of unrouted nodes.
 */
trait PenaltyForUnrouted extends VRP with Unrouted {
  /**
   * the data structure array which maintains penalty of nodes.
   */
  val weightUnroutedPenalty : Array[IntVar] = Array.tabulate(N)(i => IntVar(m, Int.MinValue, Int.MaxValue, 0,
    "penality of node " + i))
  /**
   * the variable which maintains the sum of penalty of unrouted nodes, thanks to invariant SumElements.
   */
  val UnroutedPenalty : IntVar = SumElements(weightUnroutedPenalty,unrouted)

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


trait ClosestNeighborPointsHop extends ClosestNeighborPoints with HopDistance{
  def getDistance(from: Int, to: Int):Int = getHop(from,to)
}

/**
 * Computes the nearest neighbors of each point.
 * Used by some neighborhood searches.
 */
abstract trait ClosestNeighborPoints extends VRP {

  def getDistance(from:Int,to:Int):Int
  /**
   * the data structure which maintains the k closest neighbors of each point.
   */
  var closestNeighbors:SortedMap[Int, Array[List[Int]]] = SortedMap.empty

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
   * Computes and returns the k nearest neighbor of a given node.
   * It allows us to add a filter (optional) on the neighbor we want to save.
   * @param node the given node.
   * @param k the parameter k.
   * @param filter the optional filter.
   * @return the k nearest neighbor of the a node as a list of Int.
   */
  def computeKNearestNeighbors(node:Int,k:Int,filter:(Int => Boolean) = (_=>true)):List[Int]= {

    val reachableneigbors = Nodes.filter((next:Int)
    => node != next && filter(next) && (getDistance(node,next)!= Int.MaxValue || getDistance(next, node)!= Int.MaxValue))

    val heap = new BinomialHeap[(Int,Int)](-_._2,k+1)

    for(neigbor <- reachableneigbors){
      heap.insert(neigbor, min(getDistance(neigbor, node),getDistance(node,neigbor)))
      if (heap.size>k)heap.popFirst()
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
  def getKNearest(k:Int,filter:(Int => Boolean)=(_=>true))(node:Int):Iterable[Int] = {
    if(k >= N-1) return Nodes
    if(!closestNeighbors.isDefinedAt(k)){
      saveKNearestPoints(k:Int,filter)
    }
    closestNeighbors(k)(node)
  }
}

/**
 * Maintains the hop distance in the VRP, based either on a matrix, or on another mechanism.
 * We consider that a hop distance of Int.MaxVal is unreachable.
 * HopDistance is only handling simple cost functions such as cost matrices
 */
trait HopDistance extends VRP {
  /**
   * the data structure which maintains the current hop distance of each node to reach his successor.
   * Info : the domain max is (Int.MaxValue / N) to avoid problem with domain. (allow us to use sum invariant without
   * throw over flow exception to save the distance of all vehicle).
   */
  val hopDistance = Array.tabulate(N) {(i:Int) => IntVar(m, 0, Int.MaxValue / N, 0, "hopDistanceForLeaving" + i)}

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
    for (i <- 0 until N ) hopDistance(i) <== new IntVar2IntVarFun(Next(i), j => {if (j!= N) DistanceMatrix(i)(j) else 0})
  }

  /**
   * This method sets the distance to use for the hop between points thanks
   * to a given function.
   * @param fun the function which defines the distance between two points.
   */
  def installCostFunction(fun:(Int, Int) => Int){
    distanceFunction = fun
    for (i <- 0 until N) hopDistance(i) <== new IntVar2IntVarFun(Next(i), j => fun(i,j))
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
trait HopDistanceAsObjective extends VRPObjective with HopDistance{
  addObjectiveTerm(overallDistance)
}

/**
 * Maintains the set of nodes reached by each vehicle
 */
trait NodesOfVehicle extends PositionInRouteAndRouteNr with Unrouted{
  val NodesOfVehicle = Cluster.MakeDense(RouteNr).clusters
  final override val unrouted = NodesOfVehicle(V)
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
      RouteNr(fromNode).value == RouteNr(node).value  &&
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
  val preds:Array[IntVar] = Predecessor(Next,V).preds

  //TODO: ajouter des moves plus simples, sans les neouds sprécédesseurs à chaque fois
}

/**
 * This trait maintains strong constraints system.
 * It redefines the propagation method of ObjectiveFunction trait,
 * that saves time by propagating partially.
 */
trait StrongConstraints extends VRPObjective {
  /**
   * the strong constraints system.
   */
  var strongConstraints:ConstraintSystem = new ConstraintSystem(m)

  private var strongConstraintOnLandMark:Boolean = true

  /**
   * Sets current point as comparison point for the improves method
   * @param withRestore set to true if you want the routes to be saved, so that you can restore them by calling restoreLandmark
   */
  override def defineLandmark(withRestore: Boolean) {
    strongConstraintOnLandMark = strongConstraints.isTrue
    super.defineLandmark(withRestore)
  }

  /**
   * compares the current solution with the landmark
   * returns the delta on the objectiveFunction
   * @return
   */
  override def deltaFromLandMark: Int = {
    val strongConstraint = strongConstraints.isTrue
    if (strongConstraintOnLandMark && !strongConstraint){
      Int.MaxValue
    }else if(!strongConstraintOnLandMark && strongConstraint){
      Int.MinValue
    }else{
      super.deltaFromLandMark
    }
  }
}

/**
 * This trait maintains weak constraints system.
 */
trait WeakConstraints extends VRPObjective {
  /**
   * the weak constraints system.
   */
  val weakConstraints:ConstraintSystem = new ConstraintSystem(m)

  this.addObjectiveTerm(weakConstraints)
}
