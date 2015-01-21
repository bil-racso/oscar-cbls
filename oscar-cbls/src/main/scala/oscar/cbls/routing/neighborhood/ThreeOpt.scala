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

import oscar.cbls.routing.model._
import oscar.cbls.search.SearchEngineTrait

/**
 * Removes three edges of routes, and rebuilds routes from the segments.
 * Finds 3 candidate points for a 3-opt move, and then
 * chooses on-the-fly between simple 3-opt move and reverse 3-opt move.
 *
 * Info : it also could be saw as the move of a route's segment to another place.
 * The search complexity is O(n³).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
object ThreeOpt extends Neighborhood with SearchEngineTrait {
  val REVERSE = true // this is a constant used for readability

  // PRE-CONDITION: all nodes of search zone must be routed
  override protected def doSearch(s: SearchZone,
                                  moveAcceptor: (Int) => (Int) => Boolean,
                                  returnMove: Boolean): SearchResult = {
    val vrp = s.vrp
    val startObj: Int = vrp.getObjective()

    /**
     * The insertion point is picked from the primaryNodeIterator.
     */
    while (s.primaryNodeIterator.hasNext) {
      val insertionPoint: Int = s.primaryNodeIterator.next()
      assert(vrp.isRouted(insertionPoint),
        "ThreeOpt should be applied to routed nodes only.")
        
      /**
       * The segment predecessor point (beforeStart) is picked from the insertion point
       * neighbors. It must not be the same as the insertion point, because the move would
       * not change the route.
       * 
       * The segment start point is the successor of beforeStart, it must not be a depot, neither
       * the same point.
       */
      // format: OFF (to prevent eclipse from formatting the following lines)
      for (beforeStart <- s.relevantNeighbors(insertionPoint)) {
           if (beforeStart != insertionPoint) {
           val segStartPoint = vrp.next(beforeStart).value
           if (segStartPoint != beforeStart && !vrp.isADepot(segStartPoint)) {
            // format: ON
             
            /**
             * The segment end point is picked from the next nodes of its start point route.
             */
            var segEndPoint = vrp.next(segStartPoint).value
            var afterEnd = vrp.next(segEndPoint).value
            while (segEndPoint != beforeStart && !vrp.isADepot(segEndPoint)) {
              // isBetween(i, b, a) checks i:[b, a[  (and i is in the same route as b and a)
              // we can't test with afterEnd instead of segEndPoint because it would not work
              // when afterEnd = beforeStart
              if (!vrp.isBetween(insertionPoint, beforeStart, segEndPoint)
                && insertionPoint != segEndPoint) {
                
                chooseBest3Opt(beforeStart, segStartPoint, segEndPoint, insertionPoint,
                  startObj, returnMove, moveAcceptor, vrp) match {
                    case NoMoveFound() => ()
                    case moveResult => return moveResult
                  }
              }

              segEndPoint = afterEnd
              afterEnd = vrp.next(segEndPoint).value
            }
          }
        }
      }
    }
    NoMoveFound()
  }

  /**
   * Do a 3-opt move which is : cuts a segment and inserts it (reversed if necessary)
   * in another place.
   *
   * beforeStart->[...->segEndPoint]
   * becomes
   * insertionPoint->[...->segEndPoint]
   * or
   * insertionPoint->[segEndPoint->...]
   */
  def do3opt(beforeStart: Int, segEndPoint: Int, insertionPoint: Int,
             reverseSegment: Boolean, vrp: VRP with MoveDescription) {
    var seg = vrp.cut(beforeStart, segEndPoint)
    if (reverseSegment) {
      seg = vrp.reverse(seg)
    }
    vrp.insert(seg, insertionPoint)
  }

  /**
   * Reverse a routed segment in its right place.
   */
  def reverseSegment(beforeStart: Int, segEndPoint: Int, vrp: VRP with MoveDescription) {
    val seg = vrp.cut(beforeStart, segEndPoint)
    val revSeg = vrp.reverse(seg)
    vrp.insert(revSeg, beforeStart)
  }

  /**
   * Tries a 3-opt move, then reverses the cut segment, and returns the best of the two moves,
   * or nothing if none was improving.
   */
  def chooseBest3Opt(beforeStart: Int, segStartPoint: Int, segEndPoint: Int,
                     insertionPoint: Int, startObj: Int, returnMove: Boolean,
                     moveAcceptor: (Int) => (Int) => Boolean,
                     vrp: VRPObjective with MoveDescription): SearchResult = {
    var obj = startObj
    var simple3OptImproves = false

    /**
     * Function for rolling back.
     * Rolling back from a 3-opt move with inversion, is a 3-opt move with inversion.
     */
    def rollback() {
      vrp.cleanRecordedMoves
      do3opt(insertionPoint, segStartPoint, beforeStart, REVERSE, vrp)
      vrp.commit(false)
    }

    /**
     * Function for returning the move corresponding to the selected points.
     * It is either a 3-opt or a reverse 3-opt.
     */
    def threeOptFound(reverse: Boolean) = {
      MoveFound(ThreeOpt(beforeStart, segEndPoint, insertionPoint, reverse, vrp, obj))
    }

    /**
     * FIRST, we do a simple 3-opt move,
     * with UNDO DEACTIVATED,
     * and we save if such a move is improving.
     */
    do3opt(beforeStart, segEndPoint, insertionPoint, !REVERSE, vrp)
    vrp.commit(false)

    if (moveAcceptor(obj)(vrp.getObjective)) {
      simple3OptImproves = true
      obj = vrp.getObjective
    }

    /**
     * SECOND, we reverse the moved segment, in place,
     * with UNDO ACTIVATED, so that we can go back to the previous move if necessary
     */
    reverseSegment(insertionPoint, segEndPoint, vrp) // REVERSE
    vrp.commit(true)

    if (moveAcceptor(obj)(vrp.getObjective)) {
      /**
       * Case reverse 3-opt move is better
       */
      obj = vrp.getObjective
      if (returnMove) {
        rollback()
        threeOptFound(REVERSE)
      } else {
        vrp.cleanRecordedMoves
        MovePerformed()
      }

    } else if (simple3OptImproves) {
      /**
       * Case simple 3-opt move is better
       */
      if (returnMove) {
        rollback()
        threeOptFound(!REVERSE)
      } else {
        vrp.undo()
        MovePerformed()
      }

    } else {
      /**
       * Case no improving 3-opt move with these points
       */
      rollback()
      NoMoveFound()
    }
  }

  override def toString: String = "3-opt"
}

/**
 * Models a three-opt-move operator of a given VRP problem.
 * @param beforeStart the predecessor of the moved segment.
 * @param segEndPoint the end of the moved segment.
 * @param insertionPoint the place where to insert the moved segment.
 * @param reverseSegment true if the segment will be inverted before being inserted
 * @param objAfter the objective value if we performed this three-opt-move operator.
 * @param vrp the given VRP problem.
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class ThreeOpt(beforeStart: Int, segEndPoint: Int, insertionPoint: Int,
                    reverseSegment: Boolean, override val vrp: MoveDescription,
                    override val objAfter: Int) extends Move(objAfter, vrp) {

  // overriding methods
  override def encodeMove() {
    ThreeOpt.do3opt(beforeStart, segEndPoint, insertionPoint,
      reverseSegment, vrp)
  }

  override def toString: String =
    ("TreeOpt(point before segment start = " + beforeStart
      + ", segment end point = " + segEndPoint
      + ", insertion point = " + insertionPoint
      + ", reverse segment = " + reverseSegment + ")")
}
