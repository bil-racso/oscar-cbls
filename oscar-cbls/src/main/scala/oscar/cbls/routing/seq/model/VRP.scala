package oscar.cbls.routing.seq.model

import oscar.cbls.invariants.core.algo.seq.functional.IntSequence
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.numeric.Sum
import oscar.cbls.invariants.lib.seq.{RoutingConventionMethods, ConstantRoutingDistance}
import oscar.cbls.objective.Objective

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
class VRP(val n: Int, val v: Int, val m: Store) {

  val seq = new CBLSSeqVar(m, IntSequence(0 until v), n-1, "routes")

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
  //TODO: have a O(1) algo in a trait based on partition
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

  def onTheSameRoute(node1:Int,node2:Int):Boolean = false

}

trait ConstantDistancePerVehicle extends TotalConstantDistance{
  var distancePerVehicle:Array[CBLSIntVar] = null

  override def setSymmetricDistanceMatrix(symmetricDistanceMatrix:Array[Array[Int]]){
    require(distancePerVehicle == null)
    distancePerVehicle = ConstantRoutingDistance(seq, v ,false,symmetricDistanceMatrix,true)
    totalDistance = Sum(distancePerVehicle)
  }

  override def setAsymmetricDistanceMatrix(asymetricDistanceMatrix:Array[Array[Int]]){
    require(distancePerVehicle == null)
    distancePerVehicle = ConstantRoutingDistance(seq, v ,false,asymetricDistanceMatrix,false)
    totalDistance = Sum(distancePerVehicle)
  }
}

trait TotalConstantDistance extends VRP{
  var totalDistance:IntValue = null

  def setSymmetricDistanceMatrix(symmetricDistanceMatrix:Array[Array[Int]]){
    require(totalDistance == null)
    totalDistance = ConstantRoutingDistance(seq, v ,false,symmetricDistanceMatrix,true)(0)
  }

  def setAsymmetricDistanceMatrix(asymetricDistanceMatrix:Array[Array[Int]]){
    require(totalDistance == null)
    totalDistance = ConstantRoutingDistance(seq, v ,false,asymetricDistanceMatrix,false)(0)
  }
}

/**
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 */
trait VRPObjective extends VRP {

  val accumulationVariable = CBLSIntVar(m, 0, FullRange, "objective of VRP")
  val objectiveFunction = Objective(accumulationVariable)

  var objectiveFunctionTerms: List[IntValue] = List.empty

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
  def closeObjectiveFunction() {
    if (objectiveFunctionTerms.isEmpty) throw new Error("you have set an Objective function to your VRP, but did not specify any term for it, call vrp.addObjectiveTerm, or add an objective trait to your VRP")
    accumulationVariable <== Sum(objectiveFunctionTerms)
  }

  def getObjective(): Objective = objectiveFunction
}