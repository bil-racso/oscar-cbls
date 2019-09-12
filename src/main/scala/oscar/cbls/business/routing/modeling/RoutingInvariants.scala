package oscar.cbls.business.routing.modeling

import oscar.cbls._
import oscar.cbls.core._
import oscar.cbls.business.routing.invariants._

/**
 * Created by rdl on 11L-09L-1L7.
 */
trait RoutingInvariants {

  /**
   * The distance computed by this invariant considers the values o the diagonal as part of the cost (node cost are added to the distance)
   *
   * This invariant relies on the vehicle model assumption:
   * there are v vehicles
   * They are supposed to start from point of values 0L to v-1L
   * These values must always be present in the sequence in increasing order
   * they cannot be included within a moved segment
   *
   * @param routes the routes of all the vehicles
   * @param n
   * @param v the number of vehicles in the model
   * @param perVehicle true if you request the distance per vehicle (slower)
   *                   false if you want the total distance among all vehicles
   * @param distanceMatrix the matrix of distance
   * @param distanceIsSymmetric true if you swear that the distance matrix is symmetric, false and it will be considered as asymmetric (slower!)
   * @param precomputeFW performs forward pre-computation, only useful if multiple vehicle and per vehicle distance or asymetric matrix
   * @param precomputeBW performs backward pre-computation, only useful if assymetric matrix
   * @return if the distance is PerVehicle, it is an array that maps each vehicle to its drive distance
   *         if the distance is global, it is a array with a single variable that is equal to the sum of all drive distance
   */
  def routeLength(routes : ChangingSeqValue,
                  n : Long,
                  v : Long,
                  perVehicle : Boolean,
                  distanceMatrix : Array[Array[Long]],
                  distanceIsSymmetric : Boolean,
                  precomputeFW : Boolean = false,
                  precomputeBW : Boolean = false) : Array[CBLSIntVar] =
    RouteLength(routes,
      n,
      v,
      perVehicle,
      distanceMatrix,
      distanceIsSymmetric,
      precomputeFW,
      precomputeBW)

  /**
   * maintains the set of vehicle that have at least one point to visit (beyond their start point)
   * this invariant relies on the routing convension.
   * @param routes the routes
   * @param v the number of vehicle
   */
  def movingVehicles(routes : ChangingSeqValue, v : Long) =
    MovingVehicles(routes, v)

  /**
   * this invariant maintains a degree of violation for route restriction constraints.
   * there is a set of obligation node->set of vehicles (it must reach one vehicle among any of the given set, or be non-routed)
   * the invariant maintains, for each vehicle, the number of node
   * that it reaches although it should not, according to the mentioned restrictions.
   * we consider that a node that is not routed does not violate the obligation constraint
   * @param routes
   * @param v the number of vehicles
   * @param n the number of nodes
   * @param nodeVehicleObligation the obligation that we are monitoring
   * @return an array telling the violation per vehicle
   * @note this is a preliminary naive version of the constraint. a faster one is to be developed!
   */
  def nodeVehicleObligation(routes : ChangingSeqValue, v : Long, n : Long, nodeVehicleObligation : Map[Long, Set[Long]]) : Array[CBLSIntVar] =
    NodeVehicleObligation(routes, v, n, nodeVehicleObligation)

  /**
   * this invariant maintains a degree of violation for route restriction constraints.
   * there is a set of restrictions node<->vehicle,
   * the invariant maintains, for each vehicle, the number of node
   * that it reaches although it should not, according to the mentioned restrictions.
   * @param routes
   * @param v
   * @param nodeVehicleRestrictions the restrictions that we are monitoring
   * @return an array telling the violation per vehicle
   */
  def nodeVehicleRestrictions(routes : ChangingSeqValue, v : Long, nodeVehicleRestrictions : Iterable[(Long, Long)]) =
    NodeVehicleRestrictions(routes, v, nodeVehicleRestrictions)


  /**
   * This array will maintain successorValues and successorValues to be the successor and predecessors of nodes in the route
   * it comes handy to spcify constraints or objectives. But it really slows down your model
   * @param routes a sequece representing the route, according to the routing convention
   * @param v the number of vehicles
   * @param successorValues an array of CBLSIntVar that are maintained to map node to the next reached node, according to the sequence
   * @param predecessorValues an array of CBLSIntVar that are maintained to map node to the prev reached node, according to the sequence
   * @param defaultWhenNotInSequence the value to put in the two arrays for nodes that  are not in the sequence
   */
  def routeSuccessorAndPredecessors(routes:ChangingSeqValue,
                                    v:Int,
                                    defaultWhenNotInSequence:Long)(
    successorValues:Array[CBLSIntVar] =
    Array.tabulate(routes.maxValue + 1L)(node =>
      CBLSIntVar(routes.model,defaultWhenNotInSequence,name="successor of node" + node)),
    predecessorValues:Array[CBLSIntVar] =
    Array.tabulate(routes.maxValue + 1L)(node =>
      CBLSIntVar(routes.model,defaultWhenNotInSequence,name="predecessor of node" + node))
  ) = {
    new RouteSuccessorAndPredecessors(routes,
      v,
      successorValues,
      predecessorValues,
      defaultWhenNotInSequence)
    (successorValues,predecessorValues)
  }

  /**
   * this invariant ensures that nodesOfVehicle(p) is maintained to the nodes reached vy vehicle p according to the sequence routes.
   * @param routes a sequence value representing routes
   * @param v the number of vehicles
   * @return an array nodesOfVehicle maintained to the nodes reached y each vehicle
   */
  def nodesOfVehicle(routes:ChangingSeqValue,v:Int):Array[CBLSSetVar] =
    NodesOfVehicle(routes:ChangingSeqValue,v:Int):Array[CBLSSetVar]



  /**
   * maintains an array mapping each node to the vehicle reaching it.
   * if the node is unrouted, its corresponding vehicle is set to v
   * @param routes a sequence representing all routed
   * @param v the number of vehicles
   * @return an array that mas each node in 0..route.maxValue to the vehicle reaching it
   */
  def vehicleOfNodes(routes:ChangingSeqValue,v:Int):Array[CBLSIntVar] =
    VehicleOfNodes(routes:ChangingSeqValue,v:Int):Array[CBLSIntVar]


}