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
  //for(v <- 0 until V){Next(v) := v}
  val Nodes = 0 until N
  val Vehicles = 0 until V

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

  /*
  Returns if a point n is a depot.
   */
  def isADepot(n:Int):Boolean = { n<V }

  /*
  Returns if a point n is still routed.
   */
  def isRouted(n:Int):Boolean = {Next(n).value != N}


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
  def threeOptBOld(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    assert(Next(a).value==b && Next(c).value==d && Next(e).value==f)
    var listToUpdate:List[(IntVar,Int)] = List.empty
    listToUpdate = (Next(a),d)::listToUpdate
    listToUpdate = (Next(e),c)::listToUpdate
    listToUpdate = reverse(b,c):::listToUpdate
    listToUpdate = (Next(b),f)::listToUpdate
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

/*
*  Maintains a weight on each node to help to form constraints.
*/
trait WeightedNode extends VRP {
  val weightNode : Array[IntVar] = Array.tabulate(N)(i => new IntVar(m, Int.MinValue, Int.MaxValue, 0,
    "weight of node " + i))

  def fixWeightNode(i:Int,w:Int) { weightNode(i) := w}
  def fixWeightNode(w:Int) {weightNode.foreach(p => p := w)}
}

/**
 * Maintains a penalty weight for unrouted nodes.
 */
trait PenaltyForUnrouted extends Unrouted {
  val weightUnroutedPenalty : Array[IntVar] = Array.tabulate(N)(i => new IntVar(m, Int.MinValue, Int.MaxValue, 0,
    "penality of node " + i))
  val UnroutedPenalty : IntVar = SumElements(weightUnroutedPenalty,Unrouted)
  def fixUnroutedPenaltyWeight(i:Int,w:Int) { weightUnroutedPenalty(i) := w}
  def fixUnroutedPenaltyWeight(w:Int) {weightUnroutedPenalty.foreach(p => p := w)}
}

/**
 * Maintains the set of unrouted nodes, those whose next is N.
*/
trait Unrouted extends VRP {
  val Unrouted: IntSetVar = Filter(Next, (next: Int) => next == N)
}

/**
 * Finds the nearest neighbor of each point
 * used by some neighborhood searches
 */
trait ClosestNeighborPoints extends VRP with HopDistance{
  var closestNeighbors:SortedMap[Int, Array[List[Int]]] = SortedMap.empty
  var maxAvgUnrouted:Double = 0

  def saveKNearestPoints(k:Int,filter:(Int => Boolean) = ( _ => true)){
    if (k < N-1){
      val neighbors = Array.tabulate(N)((node:Int) => computeKNearestNeighbors(node, k,filter))
      closestNeighbors += ((k,neighbors))
    }
  }

  def computeNearestNeighbors(node:Int):List[Int] = {
    val reachableneigbors = Nodes.filter((next:Int)
      => node != next && (getHop(node,next)!= Int.MaxValue || getHop(next, node)!= Int.MaxValue))
    //TODO: this is deeply inefficient. use a lazy quicksort instead, orr a partial sort based on a heap?
    reachableneigbors.sortBy((neigbor:Int) => min(getHop(neigbor, node),getHop(node,neigbor))).toList
  }

  def computeKNearestNeighbors(node:Int,k:Int,filter:(Int => Boolean)):List[Int]= {
    computeNearestNeighbors(node).filter(filter).take(k)
  }

  def getKNearestNeighbors(k:Int, node:Int,filter:(Int => Boolean) = ( _ => true)):Iterable[Int] = {
    if (k >= N-1) return Nodes
    if(!closestNeighbors.isDefinedAt(k)){
      saveKNearestPoints(k:Int,filter)
    }
    updateMaxAvgUnrouted(k,node)
    closestNeighbors(k)(node)
  }

  def updateMaxAvgUnrouted(k:Int,node:Int){
    var avg : Double = 0
    closestNeighbors(k)(node).foreach(n => if(!isRouted(n)) avg += 1)
    if (avg/k >maxAvgUnrouted) maxAvgUnrouted = avg/k
  }

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

/**
 * Declares an objective function, attached to the VRP.
*/
trait ObjectiveFunction extends VRP with ObjectiveTrait{
  // Initialize the objective function with 0 as value
  // allow negative objective value
  setObjectiveVar(new IntVar(m, Int.MinValue, Int.MaxValue, 0, "objective of VRP"))
}

/**
 * Add functions cost to the actual objective of the VRP.
*/

trait OtherFunctionToObjective extends ObjectiveFunction {
  var AddedObjectiveFunctions:IntVar = new IntVar(m,Int.MinValue,Int.MaxValue,0,"added functions Objective")

  def recordAddedFunctions(functions: Iterable[IntVar]){
    assert(!functions.isEmpty && ObjectiveVar!= null)

    var objAdd = AddedObjectiveFunctions
    functions.foreach(f => { objAdd = objAdd + f })
    AddedObjectiveFunctions = objAdd
    objAdd = ObjectiveVar + objAdd
    setObjectiveVar(objAdd)
   }

  def recordAddedFunction(function : IntVar) = recordAddedFunctions(Array[IntVar](function))
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
   * Tells if fromNode to toNode forms a segment of route of n maximum length.
   * @param fromNode the start of route.
   * @param toNode the end of route.
   * @param n the maximum length of route.
   * @return if fromNode to toNode forms a segment of route of n maximum length.
   */
  def isAtMostAsFarAs(fromNode:Int,toNode:Int,n:Int):Boolean = {
    RouteNr(fromNode).value == RouteNr(toNode).value &&
      PositionInRoute(fromNode).value + n  >= PositionInRoute(toNode).value
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
    if(isASegment(fromNode,toNode)){
      return RouteNr(fromNode).value == RouteNr(node).value  &&
        PositionInRoute(fromNode).value <= PositionInRoute(node).value &&
          PositionInRoute(node).value < PositionInRoute(toNode).value
    }
    else false
  }

  /**
   * Tells if node n and m are on the same route ( i.e. they have the same route number).
   * @param n a given node.
   * @param m another given node.
   */
  def onTheSameRoute(n:Int,m:Int):Boolean = {
    RouteNr(n).value == RouteNr(m).value
  }




}

/**
 * Add this trait if you want to maintain the predecessors of each node in the routing.
 */
trait Predecessors extends VRP{
  val preds = Predecessor(Next)
}

/**
 * Trait maintains the weak and strong constraints systems.
 * Helps to build correctly departure's heuristics.
 */
trait StrongConstraints extends ObjectiveFunction {
  var strongConstraints:ConstraintSystem = null
  var violatedStrongConstraints = false

  def setStrongConstraints(sc:ConstraintSystem) {strongConstraints = sc}
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

trait WeakConstraints extends OtherFunctionToObjective {
  var weakConstraints:ConstraintSystem = null

  def setWeakConstraints(wc:ConstraintSystem) {weakConstraints = wc;recordAddedFunction(weakConstraints.violation)}
}


/**
 * In case of symmetric instance of VRP, it helps evaluate faster some operators
 * of neighborhood as the two-opt and the three-opt.
 * To use with precaution, only if VRP is HopDistanceAsObjective, and no added functions.
 */
trait SymmetricVRP extends HopDistance{

  def isEffectiveThreeOptA(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):Boolean = isEffectiveThreeOptB(a,b,c,d,e,f)
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

  override def threeOptA(a:Int,b:Int,c:Int,d:Int,e:Int,f:Int):List[(IntVar,Int)] = {
    if (isEffectiveThreeOptA(a,b,c,d,e,f))
      super.threeOptA(a,b,c,d,e,f)
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

