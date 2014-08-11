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

package oscar.cbls.routing.neighborhood2

import oscar.cbls.routing.model._
import oscar.cbls.search.algo.HotRestart
import oscar.cbls.search.core.EasyNeighborhood

/**
 * Swaps two points of the same or different routes.
 * The search complexity is O(n²).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
class SwapNeighborhood(NodesPrecedingNodesToMove:()=>Iterable[Int],
                       relevantNeighbors:()=>Int=>Iterable[Int],
                       val vrp: VRP with MoveDescription with VRPObjective with PositionInRouteAndRouteNr,
                       val neighborhoodName:String = "SwapNeighborhood",
                       val best:Boolean = false,
                       val hotRestart:Boolean = true) extends EasyNeighborhood(best,vrp.getObjective) {

  //the indice to start with for the exploration
  var startIndice:Int = 0

  override def exploreNeighborhood(){

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(NodesPrecedingNodesToMove(), startIndice)
      else NodesPrecedingNodesToMove()

    vrp.cleanRecordedMoves()

    val relevantNeighborsNow = relevantNeighbors()

    for (beforeMovedPoint <- iterationSchemeOnZone
         if vrp.isRouted(beforeMovedPoint)) {

      val movedPoint = vrp.next(beforeMovedPoint).value

      for (
        insertionPoint <- relevantNeighborsNow(movedPoint)
        if (vrp.isRouted(insertionPoint)
          && (!vrp.isADepot(vrp.next(insertionPoint).value))
          && beforeMovedPoint != insertionPoint
          && movedPoint != insertionPoint
          && beforeMovedPoint != vrp.next(insertionPoint).value)
          && (!vrp.isADepot(movedPoint)
          || vrp.onTheSameRoute(movedPoint, insertionPoint))
      ) {

        Swap.encode(beforeMovedPoint, insertionPoint, vrp)
        vrp.commit(true)
        val newObj = vrp.getObjective()
        vrp.undo()

        if (moveRequested(newObj)
          && submitFoundMove(Swap(beforeMovedPoint, insertionPoint, newObj, vrp, neighborhoodName))) {
          startIndice = beforeMovedPoint + 1
          return
        }
      }
    }
  }

  override def reset(): Unit = {
    startIndice = 0
  }
}

object Swap{
  def encode(fstPred: Int, sndPred: Int, vrp: VRP with MoveDescription) {
    val fstSeg = vrp.cutNodeAfter(fstPred)
    val sndSeg = vrp.cutNodeAfter(sndPred)
    vrp.insert(fstSeg, sndPred)
    vrp.insert(sndSeg, fstPred)
  }
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
                 override val vrp: VRP with MoveDescription,
                 override val neighborhoodName:String = null) extends VRPMove(objAfter, vrp, neighborhoodName) {

  def encodeMove() {
    Swap.encode(fstPred, sndPred, vrp)
  }

  override def toString: String = (
    "Swap(first point predecessor = " + fstPred
      + ", second point predecessor = " + sndPred + " )")
}
