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
 * Swaps two points of the same or different routes.
 * The search complexity is O(n²).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
object Swap extends Neighborhood with SearchEngineTrait {
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
              && (!vrp.isADepot(vrp.next(insertionPoint).value))
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
              if (returnMove) {
                val move = Swap(beforeMovedPoint, insertionPoint, newObj, vrp)
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

  def encode(fstPred: Int, sndPred: Int, vrp: VRP with MoveDescription) {
    val fstSeg = vrp.cutNodeAfter(fstPred)
    val sndSeg = vrp.cutNodeAfter(sndPred)
    vrp.insert(fstSeg, sndPred)
    vrp.insert(sndSeg, fstPred)
  }

  override def toString: String = "swap"
}

/**
 * Models a swap move of a given VRP problem.
 * @param fstPred the predecessor of the first point that will be swapped.
 * @param sndPred the predecessor of the second point that will be swapped.
 * @param objAfter the objective value if we performed this swap move.
 * @param vrp the given VRP problem.
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class Swap(
  fstPred: Int,
  sndPred: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  def encodeMove() {
    Swap.encode(fstPred, sndPred, vrp)
  }

  override def toString: String = (
    "Swap(first point predecessor = " + fstPred
    + ", second point predecessor = " + sndPred + " )")
}
