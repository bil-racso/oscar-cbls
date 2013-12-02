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
 *     Refactored with respect to the new architecture by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model._
import oscar.cbls.search.SearchEngineTrait

/**
 * Removes two edges of routes, and rebuilds routes from the segments. (with one reverse required)
 *
 * The search complexity is O(n²).
 */
case class TwoOptNeighborhood extends Neighborhood with SearchEngineTrait {

  /**
   * Removes two edges of a route and flips the obtained segment before
   * reconnecting it.
   * The search complexity is O(n²).
   */
  override protected def doSearch(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean, returnMove: Boolean): SearchResult = {

    val startObj: Int = s.vrp.getObjective()
    val vrp = s.vrp

    while (s.primaryNodeIterator.hasNext) {
      val fstEdgeStartPoint: Int = s.primaryNodeIterator.next()
      if (vrp.isRouted(fstEdgeStartPoint)) {

        val fstEdgeEndPoint = vrp.Next(fstEdgeStartPoint).value

        for (
          sndEdgeStartPoint <- s.relevantNeighbors(fstEdgeStartPoint) if (vrp.isRouted(sndEdgeStartPoint)
            && sndEdgeStartPoint != fstEdgeStartPoint
            && sndEdgeStartPoint != fstEdgeEndPoint
            && fstEdgeStartPoint != vrp.Next(sndEdgeStartPoint).value
            && vrp.onTheSameRoute(fstEdgeStartPoint, sndEdgeStartPoint))
        ) {

          encode(fstEdgeStartPoint, sndEdgeStartPoint, vrp)

          checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
            case (true, newObj: Int) => { //this improved
              if (returnMove) return MoveFound(TwoOptMove(fstEdgeStartPoint, sndEdgeStartPoint, newObj, vrp))
              else return MovePerformed()
            }
            case _ => ()
          }
        }
      }
    }
    NoMoveFound()
  }

  def encode(
    fstEdgeStartPoint: Int,
    sndEdgeStartPoint: Int,
    vrp: VRP with MoveDescription) {
    val seg = vrp.cut(fstEdgeStartPoint, sndEdgeStartPoint)
    val rev_seg = vrp.reverse(seg)
    vrp.insert(rev_seg, fstEdgeStartPoint)
  }
}

/**
 * Models a two-opt-move operator of a given VRP problem.
 * @param fstEdgeStartPoint the start of first edge that we remove.
 * @param sndEdgeStartPoint the start of second edge that we remove.
 * @param objAfter the objective value if we performed this two-opt-move operator.
 * @param vrp the given VRP problem.
 */
case class TwoOptMove(
  fstEdgeStartPoint: Int,
  sndEdgeStartPoint: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  override def encodeMove() {
    new TwoOptNeighborhood().encode(fstEdgeStartPoint, sndEdgeStartPoint, vrp)
  }

  override def toString: String = ("TwoOpt(first edge startpoint = "
    + vrp.Next(fstEdgeStartPoint).value
    + ", second edge startpoint = " + sndEdgeStartPoint + " )")
}
