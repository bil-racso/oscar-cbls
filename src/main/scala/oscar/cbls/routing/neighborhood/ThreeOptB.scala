/*******************************************************************************
  * This file is part of OscaR (Scala in OR).
  *
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/gpl-3.0.html
  ******************************************************************************/

/*******************************************************************************
  * Contributors:
  *     This code has been initially developed by Ghilain Florent.
  ******************************************************************************/
package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.algebra.Algebra._
import oscar.cbls.routing.model._

/**
 * Removes three edges of routes, and rebuilds routes from the segments. (without one reverse allowed)
 *
 * Info : it also could be saw as the move of a route's segment to another place (in a reverse way).
 * The search complexity is O(n³).
 */
object ThreeOptB extends SearchEngine{

  /**
   * Returns the best three-opt-move operator, i.e. which decreases the most the objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk²)
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @return the best three-opt-move operator.
   */
  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
     k:Int):Neighbor = findMove(false, vrp, k,null,vrp.N)

  /**
   * Returns the first three-opt-move operator which decreases the actual objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk²)
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @param startFrom specifies the starting point of the search procedure.
   * @return a three-opt-move operator improving objective.
   */
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
    k:Int, startFrom:Neighbor = null):Neighbor= findMove(true,vrp, k, startFrom,vrp.N)

  /**
   * Returns the best three-opt-move operator with a restriction on the length of the moved segment,
   * i.e. which decreases the most the objective value of a given VRP problem.
   * The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk²)
   * Put lengthRestricted to 3, or 4 to get a OR-opt move.
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @param lengthRestricted the length restriction on the moved segment.
   * @return the best three-opt-move operator.
   */
  def getBestMoveRestricted(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
   , k:Int,lengthRestricted:Int):Neighbor = findMove(false, vrp, k,null,lengthRestricted)


  /**
   * Returns the first three-opt-move operator with a restriction on the length of the moved segment,
   * i.e. which decreases the actual objective value of a given VRP problem.
   * The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk²)
   * Put lengthRestricted to 3, or 4 to get a OR-opt move.
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @param startFrom specifies the starting point of the search procedure.
   * @param lengthRestricted the length restriction on the moved segment.
   * @return a three-opt-move operator improving objective.
   */
  def getFirstImprovingMoveRestricted(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
    with PositionInRouteAndRouteNr, k:Int, startFrom:Neighbor = null,lengthRestricted:Int):Neighbor =
    findMove(true,vrp, k, startFrom,lengthRestricted)

  /**
   * Search procedure of a proper three-opt-move operator in a given VRP problem.
   * Desired characteristics of the operator are given as parameter.
   * The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one.
   * @param vrp the parameter of the restricting of the nearest neighbors.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @param startFrom specifies the starting point of the search procedure.
   * @param limitLength the length restriction on the moved segment.
   * @return the proper three-opt-move operator specified by the parameters.
   */
  def findMove(FirstImprove:Boolean,
               vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
               k:Int, startFrom:Neighbor = null,limitLength:Int):ThreeOptB = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int, Int)) = null

    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (insertionPoint <- 0 until vrp.N startBy hotRestart if vrp.isRouted(insertionPoint)){
      //TODO: the search procedure is actually the same as traditional three-opt, better to look first for
      //TODO: end of segment instead of start want we reverse it.

      for (beforeSegmentStart <- vrp.getKNearestNeighbors(k,insertionPoint)  if (insertionPoint != beforeSegmentStart)
        && vrp.isRouted(beforeSegmentStart))
      {
        for (segmentEnd <- vrp.getKNearestNeighbors(k,vrp.Next(insertionPoint).value)
             if(vrp.isRouted(segmentEnd) &&
               segmentEnd != insertionPoint &&
               vrp.isAtLeastAsFarAs(beforeSegmentStart, segmentEnd,2) &&
               !vrp.isBetween(insertionPoint, beforeSegmentStart, segmentEnd) &&
               vrp.isAtMostAsFarAs(beforeSegmentStart,segmentEnd,limitLength+1)))
        {
          val newObj = getObjAfterMove(beforeSegmentStart ,segmentEnd, insertionPoint, vrp)
          if (newObj < BestObj){
            if (FirstImprove)
              return ThreeOptB(beforeSegmentStart ,segmentEnd, insertionPoint, newObj, vrp)
            BestObj = newObj
            move = ((beforeSegmentStart ,segmentEnd, insertionPoint))
          }
        }
      }
    }
    if (move == null) null
    else ThreeOptB(move._1, move._2, move._3, BestObj, vrp)
  }

  /**
   * Performs a three-opt-move operator on a given VRP problem.
   * @param startOfFirstEdge the start of first edge that we remove.
   * @param startOfSecondEdge the start of second edge that we remove.
   * @param startOfThirdEdge the start of third edge that we remove.
   * @param vrp the given VRP problem.
   */
  def doMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,vrp:VRP)  {
    val toUpdate = vrp.threeOptB(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    toUpdate.foreach(t => t._1 := t._2)
  }

  /**
   * Evaluates and returns the objective after a temporary three-opt-move operator
   * thanks to ObjectiveFunction's features.
   * @param startOfFirstEdge the start of first edge that we remove.
   * @param startOfSecondEdge the start of second edge that we remove.
   * @param startOfThirdEdge the start of third edge that we remove.
   * @param vrp the given VRP problem.
   * @return the objective value if we performed this three-opt-move operator.
   */
  def getObjAfterMove(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.threeOptB(startOfFirstEdge,vrp.Next(startOfFirstEdge).value,
      startOfSecondEdge,vrp.Next(startOfSecondEdge).value,startOfThirdEdge,vrp.Next(startOfThirdEdge).value)
    vrp.getAssignVal(toUpdate)
  }
}

/**
 * Models a three-opt-move operator of a given VRP problem. *
 * @param startOfFirstEdge the start of first edge that we remove.
 * @param startOfSecondEdge the start of second edge that we remove.
 * @param startOfThirdEdge the start of third edge that we remove.
 * @param objAfter the objective value if we performed this three-opt-move operator.
 * @param vrp the given VRP problem.
 */
case class ThreeOptB(startOfFirstEdge:Int, startOfSecondEdge:Int, startOfThirdEdge:Int,
                                  objAfter:Int, vrp:VRP) extends Neighbor{
  // overridings methods
  def comit {ThreeOptB.doMove(startOfFirstEdge,startOfSecondEdge,startOfThirdEdge,vrp)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = startOfFirstEdge

  override def toString():String = "(firstEdge = " + startOfFirstEdge + ", secondEdge = " + startOfSecondEdge + ", " +
    "thirdEdge = "+ startOfThirdEdge+" )"

}


