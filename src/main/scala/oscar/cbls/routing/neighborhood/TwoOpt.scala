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
 * Removes two edges of routes, and rebuilds routes from the segments. (with one reverse required)
 *
 * The search complexity is O(n²).
 */
object TwoOpt extends SearchEngine{

  /**
   * Returns the best two-opt-move operator, i.e. which decreases the most the objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk)
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @return the best two-opt-move operator.
   */
  def getBestMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr
    ,k:Int):TwoOpt = findMove(false, vrp,k)

  /**
   * Returns the first two-opt-move operator which decreases the actual objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk)
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @param startFrom specifies the starting point of the search procedure.
   * @return a two-opt-move operator improving objective.
   */
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with ClosestNeighborPoints with PositionInRouteAndRouteNr,
    k:Int,startFrom:Neighbor = null):TwoOpt = findMove(true,vrp,k,startFrom)


  /**
   * Search procedure of a proper two-opt-move operator in a given VRP problem.
   * Desired characteristics of the operator are given as parameter.
   * The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one.
   * @param vrp the given VRP problem.
   * @param k the parameter of the restricting of the nearest neighbors.
   * @param startFrom specifies the starting point of the search procedure.
   * @return the proper two-opt-move operator specified by the parameters.
   */
  def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with ClosestNeighborPoints
    with PositionInRouteAndRouteNr, k:Int,startFrom:Neighbor = null):TwoOpt = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (firstEdge <- 0 until vrp.N startBy hotRestart if vrp.isRouted(firstEdge)){
      for(secondEdge <- vrp.getKNearestNeighbors(k,firstEdge) if ((secondEdge!= firstEdge)
          && firstEdge!=vrp.Next(secondEdge).value &&  secondEdge!=vrp.Next(firstEdge).value)
          && vrp.onTheSameRoute(firstEdge,secondEdge))
      {
        val newObj = getObjAfterMove(firstEdge,secondEdge, vrp)
        if (newObj < BestObj){
          if (FirstImprove) return TwoOpt(firstEdge,secondEdge, newObj, vrp)
          BestObj = newObj
          move = ((firstEdge, secondEdge))
        }
      }
    }
    if (move == null) null
    else TwoOpt(move._1,move._2, BestObj, vrp)
  }

  /**
   * Performs a two-opt-move operator on a given VRP problem.
   * @param firstEdge the start of first edge that we remove.
   * @param secondEdge the start of second edge that we remove.
   * @param vrp the given VRP problem.
   */
  def doMove(firstEdge:Int, secondEdge:Int, vrp:VRP){
    val toUpdate =vrp.twoOpt(firstEdge,vrp.Next(firstEdge).value,secondEdge,vrp.Next(secondEdge).value)
    toUpdate.foreach(t => t._1 := t._2)
  }


  /**
   * Evaluates and returns the objective after a temporary two-opt-move operator
   * thanks to ObjectiveFunction's features.
   * @param firstEdge the start of first edge that we remove.
   * @param secondEdge the start of second edge that we remove.
   * @param vrp the given VRP problem.
   * @return the objective value if we performed this two-opt-move operator.
   */
  def getObjAfterMove(firstEdge:Int, secondEdge:Int, vrp:VRP with ObjectiveFunction ):Int = {
    val toUpdate = vrp.twoOpt(firstEdge,vrp.Next(firstEdge).value,secondEdge,vrp.Next(secondEdge).value)
    vrp.getAssignVal(toUpdate)
   }
}
/**
 * Models a two-opt-move operator of a given VRP problem. *
 * @param firstEdge the start of first edge that we remove.
 * @param secondEdge the start of second edge that we remove.
  * @param objAfter the objective value if we performed this two-opt-move operator.
 * @param vrp the given VRP problem.
 */
case class TwoOpt(val firstEdge:Int, val secondEdge:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  // overriding methods
  def comit {TwoOpt.doMove(firstEdge, secondEdge, vrp)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = firstEdge

  override def toString():String = "(point = " + vrp.Next(firstEdge).value + ", insertion = " + secondEdge+" )"
}

