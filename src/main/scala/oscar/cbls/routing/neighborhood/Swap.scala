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
 * Swap the place of two points of the same or different route.
 * The search complexity is O(n²).
 */
object Swap extends SearchEngine{

  /**
   * Returns the best swap move, i.e. which decreases the most the objective value
   * of a given VRP problem.
   * The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   *
   * Info: The search complexity is then O(nk)
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @return the best swap move.
   */
  def getBestMove(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr
    ,relevantNeighborhoodOfNode:Int=> Iterable[Int] = null):Swap =
      findMove(false, vrp,if(relevantNeighborhoodOfNode==null) _=>vrp.Nodes else relevantNeighborhoodOfNode)

  /**
   * Returns the first swap move which decreases the actual objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   *
   * Info: The search complexity is then O(nk)
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @param startFrom specifies the starting point of the search procedure.
   * @return a swap move improving objective.
   */
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr
   ,relevantNeighborhoodOfNode:Int=> Iterable[Int] = null, startFrom:Neighbor = null):Swap =
      findMove(true,vrp,if(relevantNeighborhoodOfNode==null) _=>vrp.Nodes else relevantNeighborhoodOfNode,startFrom)

  /**
   * Search procedure of a proper swap move in a given VRP problem.
   * Desired characteristics of the operator are given as parameter.
   * The search 's complexity can be improve by restricting the search procedure
   * to the k nearest neighbors of each points.
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one.
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @param startFrom specifies the starting point of the search procedure.
   * @return the proper swap move specified by the parameters.
   */
  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr,
    relevantNeighborhoodOfNode:Int=> Iterable[Int],startFrom:Neighbor = null):Swap = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (beforeFirstSwapPoint <- 0 until vrp.N startBy hotRestart if vrp.isRouted(beforeFirstSwapPoint) &&
      !vrp.isADepot(vrp.Next(beforeFirstSwapPoint).value)){
      val firstSwapPoint = vrp.Next(beforeFirstSwapPoint).value
      for(beforeSecondSwapPoint <- relevantNeighborhoodOfNode(firstSwapPoint) if vrp.isRouted(beforeSecondSwapPoint) &&
        !vrp.isADepot(vrp.Next(beforeSecondSwapPoint).value))
      {
        if (!vrp.onTheSameRoute(beforeFirstSwapPoint,beforeSecondSwapPoint) ||
          (vrp.isASegment(beforeFirstSwapPoint,vrp.Next(firstSwapPoint).value) &&
          vrp.isASegment(beforeSecondSwapPoint,vrp.Next(beforeSecondSwapPoint).value) &&
          vrp.isASegment(vrp.Next(firstSwapPoint).value,beforeSecondSwapPoint)))
        {
          val newObj = getObjAfterMove(beforeFirstSwapPoint,beforeSecondSwapPoint, vrp)
          if (newObj < BestObj){
            if (FirstImprove) return Swap(beforeFirstSwapPoint,beforeSecondSwapPoint, newObj, vrp)
            BestObj = newObj
            move = ((beforeFirstSwapPoint, beforeSecondSwapPoint))
          }
        }
      }
    }
    if (move == null) null
    else Swap(move._1,move._2, BestObj, vrp)
  }

  /**
   * Performs a one-point-move operator on a given VRP problem.
   * @param predOfFirstSwapedPoint the predecessor of the first point that will be swapped.
   * @param predOfSecondSwappedPoint the predecessor of the second point that will be swapped.
   * @param vrp the given VRP problem.
   */
  def doMove(predOfFirstSwapedPoint:Int, predOfSecondSwappedPoint:Int, vrp:VRP){
    val toUpdate =vrp.swap(predOfFirstSwapedPoint,vrp.Next(predOfFirstSwapedPoint).value,predOfSecondSwappedPoint,
      vrp.Next(predOfSecondSwappedPoint).value)
    toUpdate.foreach(t => t._1 := t._2)
  }

  /**
   * Evaluates and returns the objective after a temporary swap move
   * thanks to ObjectiveFunction's features.
   * @param predOfFirstSwapedPoint the predecessor of the first point that will be swapped.
   * @param predOfSecondSwappedPoint the predecessor of the second point that will be swapped.
   * @param vrp the given VRP problem.
   * @return the objective value if we performed this swap move operator.
   */
  def getObjAfterMove(predOfFirstSwapedPoint:Int, predOfSecondSwappedPoint:Int, vrp:VRP with ObjectiveFunction ):Int = {
    val toUpdate =vrp.swap(predOfFirstSwapedPoint,vrp.Next(predOfFirstSwapedPoint).value,predOfSecondSwappedPoint,
      vrp.Next(predOfSecondSwappedPoint).value)
    vrp.getAssignVal(toUpdate)
  }
}

/**
 * Models a swap move of a given VRP problem.
 * @param predOfMovedPoint the predecessor of the first point that will be swapped.
 * @param PutAfter the predecessor of the second point that will be swapped.
 * @param objAfter the objective value if we performed this swap move.
 * @param vrp the given VRP problem.
 */
case class Swap(val predOfMovedPoint:Int, val PutAfter:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  // overriding methods
  def comit {Swap.doMove(predOfMovedPoint, PutAfter, vrp)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = predOfMovedPoint
  def getValuesToAssign = vrp.swap(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter,
    vrp.Next(PutAfter).value)

  override def toString():String = "(beforeFirstSwapped = " + predOfMovedPoint + ", beforeSecondSwapped = " + PutAfter+" )"
}

