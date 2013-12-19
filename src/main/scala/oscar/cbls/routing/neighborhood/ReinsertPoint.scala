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
import scala.util.Random
import oscar.cbls.search.SearchEngineTrait

/**
 * Inserts an unrouted point in a route.
 * The search complexity is O(n²).
 */
object ReinsertPoint extends Neighborhood with SearchEngineTrait {
  override protected def doSearch(
      s: SearchZone,
      moveAcceptor: (Int) => (Int) => Boolean,
      returnMove: Boolean): SearchResult = {
    val startObj: Int = s.vrp.getObjective()
    val vrp = s.vrp

    while (s.primaryNodeIterator.hasNext) {
      val beforeReinsertedPoint: Int = s.primaryNodeIterator.next()
      if (vrp.isRouted(beforeReinsertedPoint)) {
        for (
          reinsertedPoint <- s.relevantNeighbors(beforeReinsertedPoint) if (
            !vrp.isRouted(reinsertedPoint))
        ) {
          encode(beforeReinsertedPoint, reinsertedPoint, vrp)

          checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
            case (true, newObj: Int) => { //this improved
              if (returnMove) {
                return MoveFound(ReinsertPoint(beforeReinsertedPoint,
                  reinsertedPoint, newObj, vrp))
              } else return MovePerformed()
            }
            case _ => ()
          }
        }
      }
    }
    NoMoveFound()
  }

  def encode(beforeReinsertedPoint: Int, reinsertedPoint: Int, vrp: VRP with MoveDescription) {
    assert(!vrp.isRouted(reinsertedPoint))
    val s = vrp.segmentFromUnrouted(reinsertedPoint)
    vrp.insert(s, beforeReinsertedPoint)
  }
}

/**
 * Models a reinsert-point operator of a given VRP problem.
 * @param beforeReinsertedPoint the place where to insert an unrouted point.
 * @param reinsertedPoint an unrouted point.
 * @param objAfter the objective value if we performed this reinsert-point operator.
 * @param vrp the given VRP problem.
 */
case class ReinsertPoint(
  beforeReinsertedPoint: Int,
  reinsertedPoint: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  override def encodeMove() {
    ReinsertPoint.encode(beforeReinsertedPoint, reinsertedPoint, vrp)
  }

  override def toString: String = "ReinsertPoint(beforeReinsertedPoint = " + beforeReinsertedPoint + ", reinsertedPoint = " + reinsertedPoint + " )"
}
