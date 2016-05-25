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

package oscar.cbls.routing.neighborhoodS

import oscar.cbls.search.algo.HotRestart
import oscar.cbls.search.core.EasyNeighborhood

/**
 * Moves a point of a route to another place in the same or in an other route.
 * The search complexity is O(nk).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMoveS(nodesToMove: () => Iterable[Int],
                         relevantNeighbors: () => Int => Iterable[Int],
                         vrp:VRPS,
                         neighborhoodName: String = "OnePointMove",
                         best: Boolean = false,
                         hotRestart: Boolean = true,
                         allPointsToMoveAreRouted:Boolean, allRelevantNeighborsAreRouted:Boolean) extends EasyNeighborhood[OnePointMoveSMove](best, neighborhoodName) {

  val seq = vrp.seq
  val v = vrp.V
  val n = vrp.N

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood() {

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(nodesToMove(), startIndice)
      else nodesToMove()

    val explorationStart = vrp.seq.setCheckpointStatus(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      vrp.seq.rollbackToLatestCheckpoint(explorationStart, false)
      a
    }

    val relevantNeighborsNow = relevantNeighbors()

    val movedPointsIt = iterationSchemeOnZone.iterator
    while (movedPointsIt.hasNext) {
      movedPoint = movedPointsIt.next()

      if(!vrp.isADepot(movedPoint)) {
        //depots cannot be moved at all.
        seq.newValue.positionOfValue(movedPoint) match {
          case None => ;
          case Some(positionOfMovedPoint) =>
            this.positionOfMovedPoint = positionOfMovedPoint

            val insertionPointIt = relevantNeighborsNow(movedPoint).iterator
            while (insertionPointIt.hasNext) {
              newPredecessor = insertionPointIt.next()
              if (movedPoint != newPredecessor) {

                seq.newValue.positionOfValue(newPredecessor) match {
                  case None => ;
                  case Some(positionOfNewPredecessor) =>
                    this.positionOfNewPredecessor = positionOfNewPredecessor

                    encode(positionOfMovedPoint, positionOfNewPredecessor,true)

                    if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
                      startIndice = movedPoint + 1
                      return
                    }
                }
              }
            }
        }
      }
    }
    vrp.seq.rollbackToLatestCheckpoint(explorationStart, true)
  }
  var movedPoint:Int = 0
  var newPredecessor:Int = 0
  var positionOfMovedPoint:Int = 0
  var positionOfNewPredecessor:Int = 0

  override def instantiateCurrentMove(newObj: Int) =
    OnePointMoveSMove(movedPoint, positionOfMovedPoint, newPredecessor, positionOfNewPredecessor, newObj, this, neighborhoodName)

  override def reset(): Unit = {
    startIndice = 0
  }

  def encode(positionOfMovedPoint:Int, positionOfNewPredecessor:Int, fast:Boolean) {
    seq.move(positionOfMovedPoint,positionOfMovedPoint,positionOfNewPredecessor,fast)
  }
}

/**
 * Models a one-point-move operator of a given VRP problem.
 * @param newPredecessor the place where insert the moving point.
 * @param objAfter the objective value if we performed this one-point-move operator.
 * @param neighborhood the originating neighborhood
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMoveSMove(movedPoint: Int,movedPointPosition:Int,
                             newPredecessor: Int,newPredecessorPosition:Int,
                             override val objAfter: Int,
                             override val neighborhood: OnePointMoveS,
                             override val neighborhoodName: String = "OnePointMoveMove") extends VRPSMove(objAfter, neighborhood, neighborhoodName, neighborhood.vrp){

  override def impactedPoints: List[Int] = List(movedPoint,newPredecessor)

  override def commit() {
    neighborhood.encode(movedPointPosition, newPredecessorPosition,false)
  }

  override def toString: String = (
    neighborhoodNameToString + "OnePointMove(Moved point " + movedPoint
      + " after " + newPredecessor + objToString + ")")
}

