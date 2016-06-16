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

package oscar.cbls.routing.seq.neighborhood

import oscar.cbls.routing.seq.model.VRP
import oscar.cbls.search.algo.HotRestart
import oscar.cbls.search.core.EasyNeighborhood

/**
 * Moves a point of a route to another place in the same or in an other route.
 * The search complexity is O(nk).
 * @author renaud.delandtsheer@cetic.be
 * @author yoann.guyot@cetic.be
 * @author Florent Ghilain (UMONS)
 */
case class OnePointMove(nodesToMove: () => Iterable[Int],
                        relevantNeighbors: () => Int => Iterable[Int],
                        vrp:VRP,
                        neighborhoodName: String = "OnePointMove",
                        best: Boolean = false,
                        hotRestart: Boolean = true,
                        allPointsToMoveAreRouted:Boolean = true,
                        allRelevantNeighborsAreRouted:Boolean = true) extends EasyNeighborhood[OnePointMoveMove](best, neighborhoodName) {

  val seq = vrp.routes
  val v = vrp.v
  val n = vrp.n

  //the indice to start with for the exploration
  var startIndice: Int = 0

  override def exploreNeighborhood() {

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(nodesToMove(), startIndice)
      else nodesToMove()

    val startValue = seq.defineCurrentValueAsCheckpoint(true)

    def evalObjAndRollBack() : Int = {
      val a = obj.value
      seq.rollbackToCurrentCheckpoint(startValue)
      a
    }

    val relevantNeighborsNow = relevantNeighbors()

    val movedPointsIt = iterationSchemeOnZone.iterator
    while (movedPointsIt.hasNext) {
      movedPoint = movedPointsIt.next()

      if(!vrp.isADepot(movedPoint)) {
        //depots cannot be moved at all.
        startValue.positionOfAnyOccurrence(movedPoint) match {
          case None => ;//was not routed, actually
          case Some(positionOfMovedPoint) =>
            this.positionOfMovedPoint = positionOfMovedPoint

            val insertionPointIt = relevantNeighborsNow(movedPoint).iterator
            while (insertionPointIt.hasNext) {
              newPredecessor = insertionPointIt.next()
              if (movedPoint != newPredecessor) {

                startValue.positionOfAnyOccurrence(newPredecessor) match {
                  case None => ;
                  case Some(positionOfNewPredecessor) =>
                    if(positionOfNewPredecessor+1 != positionOfMovedPoint) {
                      this.positionOfNewPredecessor = positionOfNewPredecessor

                      doMove(positionOfMovedPoint, positionOfNewPredecessor)

                      if (evaluateCurrentMoveObjTrueIfStopRequired(evalObjAndRollBack())) {
                        seq.releaseCurrentCheckpointAtCheckpoint()
                        startIndice = movedPoint + 1
                        return
                      }
                    }
                }
              }
            }
        }
      }
    }
    seq.releaseCurrentCheckpointAtCheckpoint()
  }
  var movedPoint:Int = 0
  var newPredecessor:Int = 0
  var positionOfMovedPoint:Int = 0
  var positionOfNewPredecessor:Int = 0

  override def instantiateCurrentMove(newObj: Int) =
    OnePointMoveMove(movedPoint, positionOfMovedPoint, newPredecessor, positionOfNewPredecessor, newObj, this, neighborhoodName)

  override def reset(): Unit = {
    startIndice = 0
  }

  def doMove(positionOfMovedPoint:Int, positionOfNewPredecessor:Int) {
    seq.move(positionOfMovedPoint,positionOfMovedPoint,positionOfNewPredecessor,false)
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
case class OnePointMoveMove(movedPoint: Int,movedPointPosition:Int,
                            newPredecessor: Int,newPredecessorPosition:Int,
                            override val objAfter: Int,
                            override val neighborhood: OnePointMove,
                            override val neighborhoodName: String = "OnePointMoveMove")
  extends VRPSMove(objAfter, neighborhood, neighborhoodName, neighborhood.vrp){

  override def impactedPoints: Iterable[Int] = List(movedPoint,newPredecessor)

  override def commit() {
    neighborhood.doMove(movedPointPosition, newPredecessorPosition)
  }

  override def toString: String = (
    neighborhoodNameToString + "OnePointMove(Moved point " + movedPoint
      + " after " + newPredecessor + objToString + ")")
}

