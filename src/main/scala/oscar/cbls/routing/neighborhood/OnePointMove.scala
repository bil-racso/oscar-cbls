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
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Renaud De Landtsheer and Florent Ghilain.
 ******************************************************************************/

package oscar.cbls.routing.neighborhood

import oscar.cbls.search.SearchEngine
import oscar.cbls.modeling.Algebra._
import oscar.cbls.routing.model._


/**
 * Moves a point of a route to another place in the same or in an other route.
 * The search complexity is O(n²).
 */
object OnePointMove extends SearchEngine{

  /**
   * Returns the best one-point-move operator, i.e. which decreases the most the objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   *
   * Info: The search complexity is then O(nk)
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @return the best one-point-move operator.
   */
  def getBestMove(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr,
    relevantNeighborhoodOfNode:Int=> Iterable[Int] = null):OnePointMove =
      findMove(false, vrp,if(relevantNeighborhoodOfNode==null) _=>vrp.Nodes else relevantNeighborhoodOfNode)


  /**
   * Returns the first one-point-move operator which decreases the actual objective value
   * of a given VRP problem. The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   *
   * Info: The search complexity is then O(nk)
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node. Initially returns the full set of points of the VRP.
   * @return a one-point-move operator improving objective.
   */
  def getFirstImprovingMove(vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr,
    relevantNeighborhoodOfNode:(Int=> Iterable[Int])= null, startFrom:Neighbor = null):OnePointMove =
      findMove(true,vrp,if(relevantNeighborhoodOfNode == null)_=>vrp.Nodes else relevantNeighborhoodOfNode,startFrom)

  /**
   * Search procedure of a proper one-point-move operator in a given VRP problem.
   * Desired characteristics of the operator are given as parameter.
   * The search 's complexity can be improve by restricting the search procedure
   * to a limited number of neighbors k of each points.
   * @param FirstImprove if true, returns the first improving move, otherwise, searches for the best one.
   * @param vrp the given VRP problem.
   * @param relevantNeighborhoodOfNode function that returns a list of relevant nodes to explore and restrict the search
   *                                   for a given node.
   * @param startFrom specifies the starting point of the search procedure.
   * @return the proper one-point-move specified by the parameters.
    */
  private def findMove(FirstImprove:Boolean,vrp:VRP with ObjectiveFunction with PositionInRouteAndRouteNr,
    relevantNeighborhoodOfNode:Int=> Iterable[Int],startFrom:Neighbor = null):OnePointMove = {
    var BestObj:Int = vrp.ObjectiveVar.value
    var move:((Int, Int)) = null
    val hotRestart = if (startFrom == null) 0 else startFrom.startNodeForNextExploration

    for (beforeMovedPoint <- 0 until vrp.N startBy hotRestart if vrp.isRouted(beforeMovedPoint)){
      val movedPoint = vrp.Next(beforeMovedPoint).value
      for(insertionPoint <- relevantNeighborhoodOfNode(movedPoint) if (vrp.isRouted(insertionPoint)
        && beforeMovedPoint != insertionPoint && movedPoint != insertionPoint)){
          if(!vrp.isADepot(movedPoint) || (vrp.isADepot(movedPoint) && vrp.onTheSameRoute(movedPoint,insertionPoint))){
            val newObj = getObjAfterMove(beforeMovedPoint,insertionPoint, vrp)
            if (newObj < BestObj){
              if (FirstImprove) return OnePointMove(beforeMovedPoint,insertionPoint, newObj, vrp)
              BestObj = newObj
              move = ((beforeMovedPoint, insertionPoint))
            }
          }
        }
    }
    if (move == null) null
    else OnePointMove(move._1,move._2, BestObj, vrp)
  }

  /**
   * Performs a one-point-move operator on a given VRP problem.
   * @param predOfMovedPoint the predecessor of the point that moves.
   * @param PutAfter the place where insert the moving point.
   * @param vrp the given VRP problem.
   */
  def doMove(predOfMovedPoint:Int, PutAfter:Int, vrp:VRP){
     val toUpdate = vrp.moveTo(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter)
     toUpdate.foreach(t => t._1 := t._2)
   }

  /**
   * Evaluates and returns the objective after a temporary one-point-move operator
   * thanks to ObjectiveFunction's features.
   * @param predOfMovedPoint the predecessor of the point that moves.
   * @param PutAfter the place where insert the moving point.
   * @param vrp the given VRP problem.
   * @return the objective value if we performed this one-point-move operator.
   */
  def getObjAfterMove(predOfMovedPoint:Int, PutAfter:Int, vrp:VRP with ObjectiveFunction):Int = {
    val toUpdate = vrp.moveTo(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter)
    vrp.assignVal(toUpdate)
  }

}

/**
 * Models a one-point-move operator of a given VRP problem.
 * @param predOfMovedPoint the predecessor of the point that moves.
 * @param PutAfter the place where insert the moving point.
 * @param objAfter the objective value if we performed this one-point-move operator.
 * @param vrp the given VRP problem.
 */
case class OnePointMove(predOfMovedPoint:Int, PutAfter:Int, objAfter:Int, vrp:VRP) extends Neighbor{
  // overriding methods
  def comit {OnePointMove.doMove(predOfMovedPoint, PutAfter, vrp)}
  def getObjAfter = objAfter
  def startNodeForNextExploration: Int = predOfMovedPoint
  def getValuesToAssign = vrp.moveTo(predOfMovedPoint,vrp.Next(predOfMovedPoint).value,PutAfter)

  override def toString():String = "OnePointMove(point = " + vrp.Next(predOfMovedPoint).value + ", insertion = " + PutAfter+" )"
}

