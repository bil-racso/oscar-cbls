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
 *     Factorization of code by Yoann Guyot.
 * ****************************************************************************
 */

package oscar.cbls.routing.neighborhood2

import oscar.cbls.invariants.core.computation.{Variable, CBLSSetVar}
import oscar.cbls.routing.model.{PositionInRouteAndRouteNr, VRPObjective, MoveDescription, VRP}
import oscar.cbls.search.algo.HotRestart
import oscar.cbls.search.core.{SearchResult, Neighborhood, MoveFound, NoMoveFound}

/**
 * Moves a point of a route to another place in the same or in an other route.
 * The search complexity is O(nk).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 * */
class OnePointMoveNeighborhood(NodesPrecedingNodesToMove:CBLSSetVar,
                               relevantNeighbors:Int=>Iterable[Int],
                               val vrp: VRP with MoveDescription with VRPObjective with PositionInRouteAndRouteNr,
                               val neighborhoodName:String = "OnePointMove",
                               val best:Boolean = false,
                               val hotRestart:Boolean = true) extends Neighborhood {

  //the indice to start with for the exploration
  var startIndice:Int = 0

  override def getImprovingMove(acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    val startObj: Int = vrp.getObjective()
    var oldObj = if(best) Int.MaxValue else startObj
    var toReturn: SearchResult = NoMoveFound

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(NodesPrecedingNodesToMove.value, startIndice)
      else NodesPrecedingNodesToMove.value

    vrp.cleanRecordedMoves()

    for (beforeMovedPoint <- iterationSchemeOnZone
         if vrp.isRouted(beforeMovedPoint)) {

      val movedPoint = vrp.next(beforeMovedPoint).value

      for (
        insertionPoint <- relevantNeighbors(movedPoint)
        //format: OFF (to prevent eclipse from formatting the following lines)
        if (vrp.isRouted(insertionPoint)
          && beforeMovedPoint != insertionPoint
          && movedPoint != insertionPoint
          && beforeMovedPoint != vrp.next(insertionPoint).value)
          && (!vrp.isADepot(movedPoint) || (vrp.onTheSameRoute(movedPoint, insertionPoint)))) {

        OnePointMove.encode(beforeMovedPoint, insertionPoint, vrp)

        vrp.commit(true)
        val newObj = vrp.getObjective
        vrp.undo()

        if (best) {
          if (newObj < oldObj) {
            oldObj = newObj
            toReturn = new OnePointMove(beforeMovedPoint, movedPoint, insertionPoint, newObj, vrp, neighborhoodName)
          }
        } else if (acceptanceCriterion(oldObj, newObj)) {
          startIndice = beforeMovedPoint + 1
          if (amIVerbose) println(neighborhoodName + ": move found")
          return new OnePointMove(beforeMovedPoint, movedPoint, insertionPoint, newObj, vrp, neighborhoodName)
        }
      }
    }
    toReturn match {
      case MoveFound(m)
        if acceptanceCriterion(startObj, m.objAfter) =>
        if(amIVerbose) println(neighborhoodName + ": move found")
        toReturn
      case _ =>
        if(amIVerbose) println(neighborhoodName + ": no move found")
        toReturn
    }
  }
  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

object OnePointMove{
  def encode(predOfMovedPoint: Int,
             insertionPoint: Int,
             vrp: VRP with MoveDescription) {
    val s = vrp.cutNodeAfter(predOfMovedPoint)
    vrp.insert(s, insertionPoint)
  }
}

/**
 * Models a one-point-move operator of a given VRP problem.
 * @param predOfMovedPoint the predecessor of the point that moves.
 * @param insertionPoint the place where insert the moving point.
 * @param objAfter the objective value if we performed this one-point-move operator.
 * @param vrp the given VRP problem.
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
class OnePointMove( predOfMovedPoint: Int,
                    movedPoint:Int,
                    insertionPoint: Int,
                    override val objAfter: Int,
                    override val vrp: VRP with MoveDescription,
                    override val neighborhoodName:String = null) extends VRPMove(objAfter, vrp, neighborhoodName) {

  override def encodeMove() {
    OnePointMove.encode(predOfMovedPoint, insertionPoint, vrp)
  }

  override def toString: String = (
    neighborhoodNameToString + "OnePointMove(Moved point " + movedPoint
      + " after " + insertionPoint + objToString+ " )")
}

abstract class VRPMove(override val objAfter: Int,
                       val vrp: VRP with MoveDescription,
                       override val neighborhoodName:String = null)
  extends oscar.cbls.search.move.Move(objAfter, neighborhoodName) {

  /** to actually take the move */
  override def commit(){
    vrp.cleanRecordedMoves
    encodeMove
    vrp.commit(false)
  }

  override def touchedVariables: List[Variable] = {
    vrp.cleanRecordedMoves
    encodeMove
    val toReturn = vrp.touchedVariablesByEncodedMove
    vrp.cleanRecordedMoves
    toReturn
  }

  def encodeMove()
}
