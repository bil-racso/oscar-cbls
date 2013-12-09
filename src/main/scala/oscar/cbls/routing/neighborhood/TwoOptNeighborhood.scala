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
      val fstPred: Int = s.primaryNodeIterator.next()
      if (vrp.isRouted(fstPred)) {

        val fstEdgeStartPoint = vrp.next(fstPred).value

        for (
          sndPred <- s.relevantNeighbors(fstPred) if (vrp.isRouted(sndPred)
            && sndPred != fstPred
            && sndPred != fstPred
            && fstPred != vrp.next(sndPred).value
            && vrp.onTheSameRoute(fstPred, sndPred)) //FIXME correct ?
        ) {

          encode(fstPred, sndPred, vrp)

          checkEncodedMove(moveAcceptor(startObj), !returnMove, vrp) match {
            case (true, newObj: Int) => { //this improved
              if (returnMove) return MoveFound(TwoOptMove(fstPred, sndPred, newObj, vrp))
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
    fstPred: Int,
    sndPred: Int,
    vrp: VRP with MoveDescription) {
    val seg = vrp.cut(fstPred, sndPred)
    val rev_seg = vrp.reverse(seg)
    vrp.insert(rev_seg, fstPred)
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
  fstPred: Int,
  sndPred: Int,
  override val objAfter: Int,
  override val vrp: VRP with MoveDescription) extends Move(objAfter, vrp) {
  // overriding methods
  override def encodeMove() {
    new TwoOptNeighborhood().encode(fstPred, sndPred, vrp)
  }

  override def toString: String = ("TwoOpt(first predecessor = "
    + fstPred
    + ", second predecessor = " + sndPred + " )")
}
