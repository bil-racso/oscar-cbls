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

import oscar.cbls.search.SearchEngine
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model._
import oscar.cbls.search.SearchEngineTrait
import oscar.cbls.search.algo.HotRestart
import oscar.cbls.search.core.EasyNeighborhood

/**
 * Removes two edges of routes, and rebuilds routes from the segments. (with one reverse required)
 *
 * The search complexity is O(n²).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
class TwoOptNeighborhood(PredecesorOfFirstMovedPoint:()=>Iterable[Int],
                  relevantNeighbors:()=>Int=>Iterable[Int],
                  val vrp: VRP with MoveDescription with VRPObjective with PositionInRouteAndRouteNr,
                  val neighborhoodName:String = "TwoOptNeighborhood",
                  val best:Boolean = false,
                  val hotRestart:Boolean = true) extends EasyNeighborhood(best,vrp.getObjective) {

  //the indice to start with for the exploration
  var startIndice: Int = 0

  /**
   * Removes two edges of a route and flips the obtained segment before
   * reconnecting it.
   * The search complexity is O(n²).
   */
  override def exploreNeighborhood(): Unit = {

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(PredecesorOfFirstMovedPoint(), startIndice)
      else PredecesorOfFirstMovedPoint()

    vrp.cleanRecordedMoves()
    val relevantNeighborsNow = relevantNeighbors()

    for (fstPred <- iterationSchemeOnZone) {
      assert(vrp.isRouted(fstPred),
        "The search zone should be restricted to routed.")

      for (
        sndPred <- relevantNeighborsNow(fstPred) if (vrp.isRouted(sndPred)
        && sndPred != fstPred
        && sndPred != fstPred
        && fstPred != vrp.next(sndPred).value
        && vrp.onTheSameRoute(fstPred, sndPred))
      ) {

        TwoOpt.encode(fstPred, sndPred, vrp)

        vrp.commit(true)
        val newObj = vrp.getObjective()
        if(earlyStopRequested(newObj)){
          startIndice = fstPred + 1
          return
        }
        vrp.undo()

        if (moveRequested(newObj)
          && submitFoundMove(TwoOptMove(fstPred, sndPred, newObj, vrp, neighborhoodName))) {
          startIndice = fstPred + 1
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
object TwoOpt{
  def encode(fstPred:Int, sndPred:Int, vrp:VRP with MoveDescription) {
    val seg = vrp.cut(fstPred, sndPred)
    val rev_seg = vrp.reverse(seg)
    vrp.insert(rev_seg, fstPred)
  }
}

/**
 * Models a two-opt-move operator of a given VRP problem.
 * @param fstPred the start of first edge that we remove.
 * @param sndPred the start of second edge that we remove.
 * @param objAfter the objective value if we performed this two-opt-move operator.
 * @param vrp the given VRP problem.
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
case class TwoOptMove(
  fstPred: Int,
  sndPred: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription,
  override val neighborhoodName:String = null)
  extends VRPMove(objAfter, vrp, neighborhoodName) {
  // overriding methods
  override def encodeMove() {
    TwoOpt.encode(fstPred, sndPred, vrp)
  }

  override def toString: String = ("TwoOpt(first predecessor = "
    + fstPred
    + ", second predecessor = " + sndPred + " )")
}
