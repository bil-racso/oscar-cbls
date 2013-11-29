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
 *     Refactored (in respect with the new architecture) by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood
import oscar.cbls.search.SearchEngine
import oscar.cbls.modeling.Algebra._
import scala.util.Random
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.routing.model.{ MoveDescription, VRP }

/**
 * Removes a point of route.
 * The search complexity is O(n).
 */
object RemovePoint extends Neighborhood with SearchEngineTrait {

  override protected def doSearch(s: SearchZone, moveAcceptor: (Int) => (Int) => Boolean, returnMove: Boolean): SearchResult = {
    val startObj: Int = s.vrp.getObjective()
    val vrp = s.vrp

    while (s.primaryNodeIterator.hasNext) {
      val beforeRemovedPoint: Int = s.primaryNodeIterator.next()
      if (vrp.isRouted(beforeRemovedPoint)) {

        val removedPoint = vrp.Next(beforeRemovedPoint).value

        if (vrp.isRouted(removedPoint)
          && (!vrp.isADepot(removedPoint))) {

          encode(beforeRemovedPoint, vrp)

          checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
            case (true, newObj: Int) => { //this improved
              if (returnMove) return MoveFound(RemovePoint(beforeRemovedPoint, newObj, vrp))
              else return MovePerformed()
            }
          }
        }
      }
    }
    NoMoveFound()
  }

  override def encode(beforeRemovedPoint: Int, vrp: VRP with MoveDescription) {
    vrp.unroute(vrp.cutNodeAfter(beforeRemovedPoint))
  }
}

/**
 * Models a remove-point operator of a given VRP problem.
 * @param beforeRemovedPoint the predecessor of the point that will be removed.
 * @param objAfter the objective value if we performed this remove-point operator.
 * @param vrp the given VRP problem.
 */
case class RemovePoint(
  beforeRemovedPoint: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  override def encodeMove() {
    RemovePoint.encode(beforeRemovedPoint, vrp)
  }
  override def toString: String = "RemovePoint(point = " + vrp.Next(beforeRemovedPoint).value + " )"
}
