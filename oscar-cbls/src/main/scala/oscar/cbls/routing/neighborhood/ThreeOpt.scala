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
 * Removes three edges of routes, and rebuilds routes from the segments.
 * (without any reverse)
 *
 * Info : it also could be saw as the move of a route's segment to another place.
 * The search complexity is O(n³).
 */
object ThreeOpt extends Neighborhood with SearchEngineTrait {
  val REVERSE = true

  // PRE-CONDITION: all nodes of search zone must be routed
  override protected def doSearch(s: SearchZone,
                                  moveAcceptor: (Int) => (Int) => Boolean,
                                  returnMove: Boolean): SearchResult = {
    val vrp = s.vrp
    val startObj: Int = vrp.getObjective()

    /**
     * The insertion point (trdEdgeStartPoint) is picked from the primaryNodeIterator.
     */
    while (s.primaryNodeIterator.hasNext) {
      val trdEdgeStartPoint: Int = s.primaryNodeIterator.next()
      assert(vrp.isRouted(trdEdgeStartPoint),
        "ThreeOpt should be applied to routed nodes only.")

      /**
       * The segment start point (fstEdgeStartPoint) is picked
       * from the insertion point neighbors.
       */
      // format: OFF (to prevent eclipse from formatting the following lines)
      for (fstEdgeStartPoint <- s.relevantNeighbors(trdEdgeStartPoint)
           if fstEdgeStartPoint != trdEdgeStartPoint;
           if fstEdgeStartPoint != vrp.next(trdEdgeStartPoint).value;
           fstEdgeEndPoint = vrp.next(fstEdgeStartPoint).value;
           if fstEdgeEndPoint != fstEdgeStartPoint;
           if !vrp.isADepot(fstEdgeEndPoint)) {
        // format: ON

        /**
         * The segment end point (sndEdgeStartPoint) is picked
         * from the next nodes of its start point route.
         */
        var sndEdgeStartPoint = vrp.next(fstEdgeEndPoint).value
        if (sndEdgeStartPoint != fstEdgeStartPoint) {

          var sndEdgeEndPoint = vrp.next(sndEdgeStartPoint).value
          while (sndEdgeEndPoint != fstEdgeStartPoint
            && !vrp.isADepot(sndEdgeStartPoint)
            && !vrp.isADepot(sndEdgeEndPoint)) {
            if (!vrp.isBetween(trdEdgeStartPoint, fstEdgeStartPoint, sndEdgeEndPoint)) {

              def threeOptMove(reverse: Boolean) =
                (fstEdgeStartPoint, sndEdgeStartPoint, trdEdgeStartPoint, reverse)
              val threeOptMoves = List(threeOptMove(!REVERSE), threeOptMove(REVERSE))

              selectBestMove(threeOptMoves, startObj, moveAcceptor, vrp) match {
                case (Some(move), newObj: Int) => {
                  if (returnMove) return MoveFound(move)
                  else {
                    move.encodeMove
                    vrp.commit(true)
                    vrp.cleanRecordedMoves
                    return MovePerformed()
                  }
                }
                case _ => ()
              }
            }

            sndEdgeStartPoint = sndEdgeEndPoint
            sndEdgeEndPoint = vrp.next(sndEdgeStartPoint).value
          }
        }
      }
    }
    NoMoveFound()
  }

  def encode(fstEdgeStartPoint: Int, sndEdgeStartPoint: Int, trdEdgeStartPoint: Int,
             reverseSegment: Boolean, vrp: VRP with MoveDescription) {
    var seg = vrp.cut(fstEdgeStartPoint, sndEdgeStartPoint)
    if (reverseSegment) {
      seg = vrp.reverse(seg)
    }
    vrp.insert(seg, trdEdgeStartPoint)
  }

  def selectBestMove(specs: List[(Int, Int, Int, Boolean)],
                     startObj: Int,
                     moveAcceptor: (Int) => (Int) => Boolean,
                     vrp: VRPObjective with MoveDescription): (Option[Move], Int) = {
    var obj = startObj
    var newObj = obj + 1
    var improvingMove: Option[Move] = None

    for ((fstEdgeStartPoint, sndEdgeStartPoint, trdEdgeStartPoint, reverseSegment) <- specs) {
      encode(fstEdgeStartPoint, sndEdgeStartPoint, trdEdgeStartPoint, reverseSegment, vrp)
      vrp.commit(true)
      newObj = vrp.getObjective
      val accept = moveAcceptor(obj)(newObj)
      if (accept) {
        obj = newObj
        improvingMove = Some(ThreeOpt(fstEdgeStartPoint, sndEdgeStartPoint, trdEdgeStartPoint,
          reverseSegment, vrp: MoveDescription, obj))
      }
      vrp.undo(false)
    }

    (improvingMove, obj)
  }

  override def toString: String = "3-opt"
}

/**
 * Models a three-opt-move operator of a given VRP problem.
 * @param fstEdgeStartPoint the predecessor of the moved segment.
 * @param sndEdgeStartPoint the end of the moved segment.
 * @param trdEdgeStartPoint the place where to insert the moved segment.
 * @param objAfter the objective value if we performed this three-opt-move operator.
 * @param vrp the given VRP problem.
 */
case class ThreeOpt(fstEdgeStartPoint: Int, sndEdgeStartPoint: Int, trdEdgeStartPoint: Int,
                    reverseSegment: Boolean, override val vrp: MoveDescription,
                    override val objAfter: Int) extends Move(objAfter, vrp) {

  // overriding methods
  override def encodeMove() {
    ThreeOpt.encode(fstEdgeStartPoint, sndEdgeStartPoint, trdEdgeStartPoint,
      reverseSegment, vrp)
  }

  override def toString: String =
    ("TreeOpt(first edge start point = " + fstEdgeStartPoint
      + ", second edge start point = " + sndEdgeStartPoint
      + ", third edge start point = " + trdEdgeStartPoint
      + ", reverse segment = " + reverseSegment + ")")
}
