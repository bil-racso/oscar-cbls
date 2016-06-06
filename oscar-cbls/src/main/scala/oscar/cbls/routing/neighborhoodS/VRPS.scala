package oscar.cbls.routing.neighborhoodS

import oscar.cbls.invariants.core.algo.seq.functional.{IntSequence, UniqueIntSequence}
import oscar.cbls.invariants.core.computation.{CBLSSeqVar, Store}

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
 */
class VRPS(val N: Int, val V: Int, val m: Store) {

  val seq = new CBLSSeqVar(m, IntSequence(0 until V), N-1, "routes")

  /**unroutes all points of the VRP*/
  def unroute() {
    seq := IntSequence(0 until V)
  }

  /**
   * the range of nodes (customers and deposits including) of the problem.
   */
  val nodes = 0 until N
  /**
   * the range vehicle of the problem.
   */
  val vehicles = 0 until V

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
  //TODO: have a O(1) algo in a trait based on partition
  def isRouted(n: Int): Boolean = {seq.value.contains(n)}

  /**
   * This function is intended to be used for testing only.
   * setCircuit(List(1,2,3,4)) produces the following route :
   * 1 -> 2 -> 3 -> 4 (-> 1)
   */
  def setCircuit(nodes: Iterable[Int]): Unit = {
    seq := IntSequence(nodes)
    for(v <- 0 until V) require(seq.newValue.contains(v))
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
    require(vehicle < V)
    var currentVExplorer = seq.newValue.explorerAtAnyOccurrence(vehicle).head.next
    var acc:List[Int] = List(vehicle)
    while (currentVExplorer match{
      case Some(x) if x.value >= V =>
        acc = x.value :: acc
        currentVExplorer = x.next
        true
      case _ => false}) {}
    acc.reverse
  }

  /**
   * Redefine the toString method.
   * @return the VRP problem as a String.
   */
  override def toString: String = {
    var toReturn = unroutedToString

    for (v <- 0 to V - 1) {
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
