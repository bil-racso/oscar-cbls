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

import oscar.cbls.routing.model._
import oscar.cbls.search.SearchEngineTrait

/**
 * Inserts an unrouted point in a route.
 * The search complexity is O(n²).
 *
 * PRE-CONDITIONS:
 * - the relevant neighbors must all be routed,
 * - the primary node iterator must contain only unrouted nodes.
 * @author renaud.delandtsheer@cetic.be
 * @author Florent Ghilain (UMONS)
 * @author yoann.guyot@cetic.be
 */
object InsertPoint extends Neighborhood with SearchEngineTrait {
  override protected def doSearch(
    s: SearchZone,
    moveAcceptor: (Int) => (Int) => Boolean,
    returnMove: Boolean): SearchResult = {
    val startObj: Int = s.vrp.getObjective()
    s.vrp.cleanRecordedMoves()
    val vrp = s.vrp

    while (s.primaryNodeIterator.hasNext) {
      val insertedPoint = s.primaryNodeIterator.next
      assert(!vrp.isRouted(insertedPoint),
        "The search zone should be restricted to unrouted nodes when inserting.")

      val routedNeighbors = s.relevantNeighbors(insertedPoint)
      for (beforeInsertedPoint <- routedNeighbors) {
        assert(vrp.isRouted(beforeInsertedPoint),
          "The relevant neighbors should be routed.")
        assert(s.vrp.isRecording, "MoveDescription should be recording now")

        encode(beforeInsertedPoint, insertedPoint, vrp)

        checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
          case (true, newObj: Int) => { //this improved
            if (returnMove) {
              return MoveFound(InsertPoint(beforeInsertedPoint,
                insertedPoint, newObj, vrp))
            } else return MovePerformed()
          }
          case _ => ()
        }
      }
    }
    NoMoveFound()
  }

  def encode(beforeInsertedPoint: Int, insertedPoint: Int, vrp: VRP with MoveDescription) {
    assert(!vrp.isRouted(insertedPoint))
    assert(vrp.isRouted(beforeInsertedPoint))
    val s = vrp.segmentFromUnrouted(insertedPoint)
    vrp.insert(s, beforeInsertedPoint)
  }

  override def toString: String = "insertion"
}

/**
 * Models a reinsert-point operator of a given VRP problem.
 * @param beforeInsertedPoint the place where to insert an unrouted point.
 * @param insertedPoint an unrouted point.
 * @param objAfter the objective value if we performed this reinsert-point operator.
 * @param vrp the given VRP problem.
 */
case class InsertPoint(
  beforeInsertedPoint: Int,
  insertedPoint: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  override def encodeMove() {
    InsertPoint.encode(beforeInsertedPoint, insertedPoint, vrp)
  }

  override def toString: String = "InsertPoint(beforeInsertedPoint = " + beforeInsertedPoint + ", insertedPoint = " + insertedPoint + " )"
}
