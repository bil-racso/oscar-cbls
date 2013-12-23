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
 *
 *     Refactored with respect to the new architecture by Yoann Guyot
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model._
import oscar.cbls.search.SearchEngineTrait

/**
 * Removes three edges of routes, and rebuilds routes from the segments. (without any reverse)
 *
 * Info : it also could be saw as the move of a route's segment to another place.
 * The search complexity is O(n³).
 */
object ThreeOpt extends Neighborhood with SearchEngineTrait {

  override protected def doSearch(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean, returnMove: Boolean): SearchResult = {

    val startObj: Int = s.vrp.getObjective()
    val vrp = s.vrp

    while (s.primaryNodeIterator.hasNext) {
      val fstEdgeStartPoint: Int = s.primaryNodeIterator.next()
      if (vrp.isRouted(fstEdgeStartPoint)) {

        val fstEdgeEndPoint = vrp.next(fstEdgeStartPoint).value
        if (fstEdgeEndPoint != fstEdgeStartPoint) {

          var sndEdgeStartPoint = vrp.next(fstEdgeEndPoint).value
          if (sndEdgeStartPoint != fstEdgeStartPoint
            && vrp.next(sndEdgeStartPoint).value != fstEdgeStartPoint) {
            while (vrp.next(sndEdgeStartPoint).value != fstEdgeStartPoint) {
              //            && !vrp.isADepot(sndEdgeStartPoint)) {
              val sndEdgeEndPoint = vrp.next(sndEdgeStartPoint).value

              var trdEdgeStartPoint = sndEdgeEndPoint
              while (trdEdgeStartPoint != fstEdgeStartPoint) {
                if (!vrp.isBetween(trdEdgeStartPoint, fstEdgeStartPoint, sndEdgeEndPoint)) {
                  //             && !vrp.isADepot(trdEdgeStartPoint)) {

                  encode(fstEdgeStartPoint, sndEdgeStartPoint, trdEdgeStartPoint, vrp)

                  checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
                    case (true, newObj: Int) => { //this improved
                      if (returnMove) return MoveFound(ThreeOpt(fstEdgeStartPoint, sndEdgeStartPoint, trdEdgeStartPoint, newObj, vrp))
                      else return MovePerformed()
                    }
                    case _ => ()
                  }
                  trdEdgeStartPoint = vrp.next(trdEdgeStartPoint).value
                }
              }
              sndEdgeStartPoint = vrp.next(sndEdgeStartPoint).value
            }
          }
        }
      }
    }
    NoMoveFound()
  }

  def encode(
    fstEdgeStartPoint: Int,
    sndEdgeStartPoint: Int,
    trdEdgeStartPoint: Int,
    vrp: VRP with MoveDescription) {
    val seg = vrp.cut(fstEdgeStartPoint, sndEdgeStartPoint)
    vrp.insert(seg, trdEdgeStartPoint)
  }
  
  override def toString: String = "3-opt"
}

/**
 * Models a three-opt-move operator of a given VRP problem.
 * @param beforeSegmentStart the predecessor of the moved segment.
 * @param segmentEnd the end of the moved segment.
 * @param insertionPoint the place where to insert the moved segment.
 * @param objAfter the objective value if we performed this three-opt-move operator.
 * @param vrp the given VRP problem.
 */
case class ThreeOpt(fstEdgeStartPoint: Int, sndEdgeStartPoint: Int, trdEdgeStartPoint: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  override def encodeMove() {
    ThreeOpt.encode(fstEdgeStartPoint, sndEdgeStartPoint, trdEdgeStartPoint, vrp)
  }

  override def toString: String =
    ("TreeOpt(first edge start point = " + fstEdgeStartPoint
      + ", second edge start point = " + sndEdgeStartPoint
      + ", third edge start point =" + trdEdgeStartPoint + " )")
}
