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

import oscar.cbls.routing.model._
import oscar.cbls.routing.neighborhood.InsertPoint
import oscar.cbls.routing.neighborhood.SearchZone

/**
 * Constructs an initial solution by repeatedly inserting points into the circuits.
 * it performs a round-robin on vehicles, and every time performs the best possible insert
 * by applying [[oscar.cbls.routing.neighborhood.InsertPoint]]
 */
object BestInsert {

  /**
   * It applies the initial solution to a given vrp problem.
   * @param vrp : the vrp problem that we want to apply the initial solution.
   */
  def apply(vrp: VRP with VRPObjective with PositionInRouteAndRouteNr with MoveDescription) {
    print("(BestInsert) ")
    val relevantNeighbors = (n: Int) => vrp.nodes
    while (true) {
      InsertPoint.bestImprovingMove(
        SearchZone(relevantNeighbors, vrp.nodes.iterator, vrp)) match {
          case Some(m) => m.doMove
          case None => return
        }
    }
  }
}
