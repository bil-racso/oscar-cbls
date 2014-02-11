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
 *         by Renaud De Landtsheer and Florent Ghilain.
 *     Factorization of code by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model.{ MoveDescription, VRP }

/**
 * Moves a point of a route to another place in the same or in an other route.
 * The search complexity is O(n²).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
object OnePointMove extends Neighborhood with SearchEngineTrait {

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
          insertionPoint <- s.relevantNeighbors(movedPoint)
          //format: OFF (to prevent eclipse from formatting the following lines)
          if (vrp.isRouted(insertionPoint)
              && beforeMovedPoint != insertionPoint
              && movedPoint != insertionPoint
              && beforeMovedPoint != vrp.next(insertionPoint).value)
              && (!vrp.isADepot(movedPoint)
                  || (vrp.onTheSameRoute(movedPoint, insertionPoint)))
        //format: ON
        ) {
          //          println("BOUCLE2: insertionPoint = " + insertionPoint)
          //          print("VRP before encode dans la boucle de recherche: ")
          //          println(vrp)

          encode(beforeMovedPoint, insertionPoint, vrp)

          checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
            case (true, newObj: Int) => { //this improved
              if (returnMove) return MoveFound(
                OnePointMove(beforeMovedPoint, insertionPoint, newObj, vrp))
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

  def encode(predOfMovedPoint: Int,
             insertionPoint: Int,
             vrp: VRP with MoveDescription) {
    val s = vrp.cutNodeAfter(predOfMovedPoint)
    vrp.insert(s, insertionPoint)
  }

  override def toString: String = "1-pt"
}

/**
 * Models a one-point-move operator of a given VRP problem.
 * @param predOfMovedPoint the predecessor of the point that moves.
 * @param insertionPoint the place where insert the moving point.
 * @param objAfter the objective value if we performed this one-point-move operator.
 * @param vrp the given VRP problem.
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMove(
  predOfMovedPoint: Int,
  insertionPoint: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {

  // overriding methods
  override def encodeMove() {
    OnePointMove.encode(predOfMovedPoint, insertionPoint, vrp)
  }

  override def toString: String = (
    "OnePointMove(Moved point = " + vrp.next(predOfMovedPoint).value
    + ", inserted after = " + insertionPoint + " )")
}
