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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.routing.model.MoveDescription
import oscar.cbls.routing.model.VRP
import oscar.cbls.search.SearchEngineTrait

/**
 * Moves some point of a route to another place in the same or in an other route.
 * The search complexity is O(n^2).
 */
abstract class TwoPointsNeighborhood extends Neighborhood with SearchEngineTrait {
  /**
   * Does the search and stops at first improving move.
   * @param s the search zone, including the VRP that we are examining
   * @param returnMove true: returns first improving move, false: performs first improving move
   * @return
   */
  override protected def doSearch(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean, returnMove: Boolean): SearchResult = {

    val startObj: Int = s.vrp.getObjective()
    s.vrp.cleanRecordedMoves()
    val vrp = s.vrp

    while (s.primaryNodeIterator.hasNext) {
      val beforeMovedPoint: Int = s.primaryNodeIterator.next()
//      println("BOUCLE1: beforeMovedPoint = " + beforeMovedPoint)
      if (vrp.isRouted(beforeMovedPoint)) {

        val movedPoint = vrp.next(beforeMovedPoint).value
//        println("movedPoint = " + movedPoint)

        for (
          insertionPoint <- s.relevantNeighbors(movedPoint) if (vrp.isRouted(insertionPoint)
            && beforeMovedPoint != insertionPoint
            && movedPoint != insertionPoint
            && beforeMovedPoint != vrp.next(insertionPoint).value)
            && (!vrp.isADepot(movedPoint) || (vrp.onTheSameRoute(movedPoint, insertionPoint)))
        ) {
//          println("BOUCLE2: insertionPoint = " + insertionPoint)
//          print("VRP before encode dans la boucle de recherche: ")
//          println(vrp)

          encode(beforeMovedPoint, insertionPoint, vrp)

          checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
            case (true, newObj: Int) => { //this improved
              if (returnMove) return MoveFound(getMove(beforeMovedPoint, insertionPoint, newObj, vrp))
              else {
                return MovePerformed()
              }
            }
            case (false, _) => ()
          }
        }
      }
    }
    NoMoveFound()
  }

  def getMove(beforeMovedPoint: Int, insertionPoint: Int, newObj: Int, vrp: VRP with MoveDescription): Move

  def encode(beforeMovedPoint: Int, insertionPoint: Int, vrp: VRP with MoveDescription)
}
