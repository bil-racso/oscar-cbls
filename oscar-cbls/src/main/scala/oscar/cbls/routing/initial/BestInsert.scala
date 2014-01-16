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
 *     Refactored with respect to the new architecture by Yoann Guyot
 * ****************************************************************************
 */

package oscar.cbls.routing.initial

import oscar.cbls.routing.model.MoveDescription
import oscar.cbls.routing.model.PositionInRouteAndRouteNr
import oscar.cbls.routing.model.RoutedAndUnrouted
import oscar.cbls.routing.model.VRP
import oscar.cbls.routing.model.VRPObjective
import oscar.cbls.routing.neighborhood.InsertPoint
import oscar.cbls.routing.neighborhood.SearchZone
import oscar.cbls.routing.model.ClosestNeighbors

/**
 * Constructs an initial solution by repeatedly inserting points into the circuits.
 * it performs a round-robin on vehicles, and every time performs the best possible insert
 * by applying [[oscar.cbls.routing.neighborhood.InsertPoint]]
 */
object BestInsert {

  /**
   * It applies the initial solution to a given vrp problem.
   * @param vrp : the vrp problem that we want to apply the initial solution.
   */  // format: OFF (to prevent eclipse from formatting the following lines)
  def apply(vrp: VRP with RoutedAndUnrouted with VRPObjective
                     with PositionInRouteAndRouteNr with MoveDescription
                     with ClosestNeighbors) {
    // format: ON
    apply(vrp, (n: Int) => vrp.routed.value)
  }
  
  // format: OFF (to prevent eclipse from formatting the following lines)
  def apply(vrp: VRP with RoutedAndUnrouted with VRPObjective
                     with PositionInRouteAndRouteNr with MoveDescription
                     with ClosestNeighbors, k: Int) {
    // format: ON
    val relevantNeighbors = vrp.getKNearest(k)_
    apply(vrp, (n: Int) => relevantNeighbors(n).filter(vrp.isRouted))
  }
  
  // format: OFF (to prevent eclipse from formatting the following lines)
  private def apply(vrp: VRP with RoutedAndUnrouted with VRPObjective
                             with PositionInRouteAndRouteNr with MoveDescription
                             with ClosestNeighbors, routed: Int => Iterable[Int]) {
    // format: ON
    print("Applying best insert heuristic...")

    //    for (unroutedNode <- vrp.unrouted.value) {
    //      val routed = (n: Int) => vrp.routed.value
    //      InsertPoint.climbBest(SearchZone(routed, List(unroutedNode).toIterator, vrp))
    //    }
    while (true) {
      InsertPoint.bestImprovingMove(
        SearchZone(routed, vrp.unrouted.value.toIterator, vrp)) match {
          case Some(m) => m.doMove
          case None => println(" done."); return
        }
    }
    println(" done.")
  }
}
