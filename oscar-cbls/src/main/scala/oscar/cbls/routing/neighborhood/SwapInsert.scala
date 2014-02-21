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
 *     This code has been initially developed by Ghilain Florent.
 *     Refactored in respect with the new architecture by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.search.SearchEngine
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model._

/**
 * Swaps a routed point and an unrouted point.
 * The search complexity is O(n²).
 * @author yoann.guyot@cetic.be
 */
object SwapInsert extends Neighborhood with SearchEngineTrait {
  /**
   * Does the search and stops at first improving move.
   * @param s the search zone, including the VRP that we are examining
   * @param returnMove true: returns first improving move, false: performs first improving move
   * @return
   */
  override protected def doSearch(
    s: SearchZone,
    moveAcceptor: (Int) => (Int) => Boolean,
    returnMove: Boolean): SearchResult = {

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
          unroutedPoint <- (s.relevantNeighbors(beforeMovedPoint) ++ s.relevantNeighbors(movedPoint))
          //format: OFF (to prevent eclipse from formatting the following lines)
          if (!vrp.isRouted(unroutedPoint)
              && (!vrp.isADepot(movedPoint)))
        //format: ON
        ) {
          //          println("BOUCLE2: unroutedPoint = " + unroutedPoint)
          //          print("VRP before encode dans la boucle de recherche: ")
          //          println(vrp)

          encode(beforeMovedPoint, unroutedPoint, vrp)

          checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
            case (true, newObj: Int) => { //this improved
              if (returnMove) {
                val move = SwapInsert(beforeMovedPoint, unroutedPoint, newObj, vrp)
                return MoveFound(move)
              } else {
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

  def encode(beforeMovedPoint: Int, unroutedPoint: Int, vrp: VRP with MoveDescription) {
    println("swap-insert(" + beforeMovedPoint + ", " + unroutedPoint + ")")
    val cutSeg = vrp.cutNodeAfter(beforeMovedPoint)
    vrp.unroute(cutSeg)
    val newSeg = vrp.segmentFromUnrouted(unroutedPoint)
    vrp.insert(newSeg, beforeMovedPoint)
  }

  override def toString: String = "swap-insert"
}

/**
 * Models a swap-insert move of a given VRP problem.
 * @param beforeMovedPoint the predecessor of the point that will be unrouted.
 * @param unroutedPoint the point that will be routed.
 * @param objAfter the objective value if we performed this swap-insert move.
 * @param vrp the given VRP problem.
 * @author yoann.guyot@cetic.be
 * */
case class SwapInsert(
  beforeMovedPoint: Int,
  unroutedPoint: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  def encodeMove() {
    SwapInsert.encode(beforeMovedPoint, unroutedPoint, vrp)
  }

  override def toString: String = (
    "SwapInsert(first point predecessor = " + beforeMovedPoint
    + ", point to be routed = " + unroutedPoint + " )")
}
