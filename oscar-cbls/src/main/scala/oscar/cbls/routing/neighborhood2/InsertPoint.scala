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

package oscar.cbls.routing.neighborhood2

import oscar.cbls.routing.model._
import oscar.cbls.search.algo.HotRestart
import oscar.cbls.search.core.EasyNeighborhood

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
case class InsertPoint(UnroutedNodesToInsert:()=>Iterable[Int],
                  relevantNeighbors:()=>Int=>Iterable[Int],
                  val vrp: VRP with MoveDescription with VRPObjective,
                  val neighborhoodName:String = "InsertPoint",
                  val best:Boolean = false,
                  val hotRestart:Boolean = true) extends EasyNeighborhood(best,vrp.getObjective) {

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood(): Unit = {

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(UnroutedNodesToInsert(), startIndice)
      else UnroutedNodesToInsert()

    vrp.cleanRecordedMoves()
    val relevantNeighborsNow = relevantNeighbors()

    for (insertedPoint <- iterationSchemeOnZone) {
      assert(!vrp.isRouted(insertedPoint),
        "The search zone should be restricted to unrouted nodes when inserting.")

      for (beforeInsertedPoint <- relevantNeighborsNow(insertedPoint)
           if (vrp.isRouted(beforeInsertedPoint))) {
        assert(vrp.isRecording, "MoveDescription should be recording now")

        InsertPoint.encode(beforeInsertedPoint, insertedPoint, vrp)
        vrp.commit(true)
        val newObj = vrp.getObjective()
        vrp.undo()

        if (moveRequested(newObj)
          && submitFoundMove(InsertPointMove(beforeInsertedPoint, insertedPoint, newObj, vrp, neighborhoodName))) {
          startIndice = insertedPoint + 1
          return
        }

      }
    }
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

object InsertPoint{
  def encode(beforeInsertedPoint: Int, insertedPoint: Int, vrp: VRP with MoveDescription) {
    assert(!vrp.isRouted(insertedPoint))
    assert(vrp.isRouted(beforeInsertedPoint))
    val s = vrp.segmentFromUnrouted(insertedPoint)
    vrp.insert(s, beforeInsertedPoint)
  }
}

/**
 * Models a reinsert-point operator of a given VRP problem.
 * @param beforeInsertedPoint the place where to insert an unrouted point.
 * @param insertedPoint an unrouted point.
 * @param objAfter the objective value if we performed this reinsert-point operator.
 * @param vrp the given VRP problem.
 */
case class InsertPointMove(beforeInsertedPoint: Int,
                           insertedPoint: Int,
                           override val objAfter: Int,
                           override val vrp: VRP with MoveDescription,
                           override val neighborhoodName:String = null)
  extends VRPMove(objAfter, vrp, neighborhoodName) {

  override def encodeMove() {
    InsertPoint.encode(beforeInsertedPoint, insertedPoint, vrp)
  }

  override def toString: String =
    "InsertPoint(beforeInsertedPoint = " + beforeInsertedPoint +
      ", insertedPoint = " + insertedPoint + " )"
}
